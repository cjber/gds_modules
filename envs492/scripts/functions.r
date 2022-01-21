## ---- libs
library(pacman)

pkgs <- c(
    "ENVS450",
    "devtools",
    "Hmisc",
    "PerformanceAnalytics",
    "ggthemes",
    "RStoolbox",
    "broom",
    "viridis",
    "viridisLite",
    "ggpubr",
    "magrittr",
    "sf",
    "kableExtra",
    "scales",
    "lidR",
    "raster",
    "nvimcom",
    "tidyverse",
    "varhandle",
    "future",
    "rgdal",
    "pbapply",
    "cowplot",
    "bibtex",
    "benchmarkme",
    "parallel",
    "showtext",
    "data.table",
    "wesanderson"
)

pacman::p_load(pkgs, character.only = T)

## ---- make_table
make_table <- function(df, cap = "", dig = 2, col_names = NA, table_env = "table", ...) {
    require(kableExtra)
    require(tidyverse)

    options(knitr.kable.NA = "")
    kable(df,
        digits = dig, caption = cap,
        linesep = "", # remove 5 row spacing
        longtable = FALSE, booktabs = TRUE, # latex opts
        format = "latex",
        escape = F, # allow maths chars
        col.names = col_names,
        table.env = table_env # change to figure*
    ) %>%
        kable_styling(font_size = 9, position = "center") %>%
        row_spec(0, bold = TRUE)
}

## ---- ctg_to_df
ctg_to_df <- function(cluster, aerial = NULL) {
    # read cluster as LAS
    las <- readLAS(cluster)
    # dont read empty clusters
    # all subsequent ctg funcs need these
    if (is.empty(las)) {
        return(NULL)
    }
    # to sp then tibble
    las <- las %>%
        as.spatial()

    if (is.null(aerial) == FALSE) {
        las@data$lum <- as.numeric(raster::extract(aerial, las))
    }
    # sp to df
    las <- as.data.frame(las)
    return(las)
}

## ---- clip_samples
clip_samples <- function(cluster, x) {
    las <- readLAS(cluster)
    if (is.empty(las)) {
        return(NULL)
    }
    # las to sp, sf then spatial join
    las <- las %>%
        as.spatial() %>%
        st_as_sf(las) %>%
        st_set_crs(27700) %>%
        st_join(x)

    # clip points by removing NA values
    las <- las[is.na(las$sample_id) == FALSE, ]
    return(las)
}

## ---- las_filter_noise
las_filter_noise <- function(cluster, sensitivity = 1) {
    las <- readLAS(cluster)
    if (is.empty(las)) {
        return(NULL)
    }
    # find 95th quantile intensity values per 10m^2
    p95i <- grid_metrics(las, ~ quantile(Intensity, probs = 0.95), 10)
    p95z <- grid_metrics(las, ~ quantile(Z, probs = 0.95), 10)
    # join by merging
    las <- lasmergespatial(las, p95i, "p95i")
    # remove above 95th quantile
    las <- lasfilter(las, Intensity < p95i * sensitivity)

    las <- lasmergespatial(las, p95z, "p95z")
    # remove above 95th quantile
    las <- lasfilter(las, Z < p95z * sensitivity)
    # remove unneeded var
    las$p95i <- NULL
    las$p95z <- NULL
    return(las)
}

## ---- lidr_clean
lidr_clean <- function(cluster) {
    las <- readLAS(cluster)
    if (is.empty(las)) {
        return(NULL)
    }
    # remove all but last return
    las <- lasfilter(las, NumberOfReturns == ReturnNumber)

    # find ground points
    las <- lasground(las, csf())

    ## Create Point DEM
    # interpolate ground points to create raster dtm. Uses Classification = 2
    # very large number of points, therefore idw used as opposed to kriging
    dtm <- grid_terrain(las, 1, knnidw(k = 10, p = 2))
    # normalise heights using dtm
    las <- lasnormalize(las, dtm)
    return(las)
}

## ---- extract_buff
extract_buff <- function(cluster, clip_input) {
    las <- readLAS(cluster)

    if (is.empty(las)) {
        return(NULL)
    }

    # ensure no null input
    if (!is.null(clip_input)) {
        las <- lasclip(las, clip_input)

        # bind clipped inputs together
        # as gives list depending on number of
        # sp objects
        if (length(las) > 1) {
            for (i in 1:length(las)) {
                if (!is.empty(las[[i]])) {
                    las <- do.call(rbind, las)
                    return(las)
                }
            }
        }
    }
}

## ---- find_dists
find_dists <- function(x, y) {
    # euclidean distance with sf
    d <- st_distance(x, y)
    return(d)
}

## ---- euc
# Function to calculate Euclidean distance between 2 points
# using coordinate data
euclidean_distance <- function(p1, p2) {
    return(sqrt((p2[1] - p1[1])**2 + (p2[2] - p1[2])**2))
}

## ---- perp
# Function to calculate 2 points on a line perpendicular to another defined by 2 points p1,p2
# For point at interval, which can be a proportion of the segment length, or a constant
# At distance n from the source line
calc_perp <- function(p1, p2, n, interval = 0.5, proportion = TRUE) {
    # Calculate x and y distances
    x_len <- p2[1] - p1[1]
    y_len <- p2[2] - p1[2]

    # If proportion calculate reference point from tot_length
    if (proportion) {
        point <- c(p1[1] + x_len * interval, p1[2] + y_len * interval)
    }
    # Else use the constant value
    else {
        tot_len <- euclidean_distance(p1, p2)
        point <- c(
            p1[1] + x_len / tot_len * interval,
            p1[2] + y_len / tot_len * interval
        )
    }

    # Calculate the x and y distances from reference point
    # to point on line n distance away
    ref_len <- euclidean_distance(point, p2)
    xn_len <- (n / ref_len) * (p2[1] - point[1])
    yn_len <- (n / ref_len) * (p2[2] - point[2])

    # Invert the x and y lengths and add/subtract from the refrence point
    ref_points <- rbind(
        point,
        c(point[1] + yn_len, point[2] - xn_len),
        c(point[1] - yn_len, point[2] + xn_len)
    )

    # Return the reference points
    return(ref_points)
}

## ---- comb_ctg
comb_ctg <- function(x) {
    las <- readLAS(x)
    if (is.empty(las)) {
        return(NULL)
    }
    return(las)
}

## ---- compute_samples
# default of 10m increments and 30m width either side of a line
compute_samples <- function(x, increment = 10, width = 30) {
    sample_lines <- c()
    if (nrow(x) > 1) {
        # split linestring into coordinates
        road_node <- st_coordinates(x)
        tot_len <- 0
        len_inc <- increment
        len_ofs <- len_inc

        # for each linestring "node"
        # find dist between them
        for (i in 2:nrow(road_node) - 1) {
            n1 <- road_node[i, ]
            n2 <- road_node[i + 1, ]

            len_seg <- euclidean_distance(n1, n2)
            len_ofs <- len_ofs + len_inc

            # max length of linestring
            while (len_ofs <= tot_len + len_seg) {
                len_ofs <- len_ofs + len_inc

                # Add results to output vector
                # for each node of a linestring
                perp_segments <- calc_perp(
                    n1, n2, width,
                    len_ofs - tot_len,
                    proportion = FALSE
                )

                # combine to multipts
                multipoints <- st_multipoint(matrix(perp_segments, ncol = 2))
                pts <- st_cast(st_geometry(multipoints), "POINT")
                n <- length(pts)

                # points to perp lines
                pair <- st_combine(c(pts[1], pts[2], pts[3]))
                # then to linestring + buffer to polygon
                linestring <- st_cast(pair, "LINESTRING") %>%
                    st_buffer(2) %>%
                    st_sf() %>%
                    mutate(road_id = as.character(unique(x$road_id)))
                sample_lines <- rbind(sample_lines, linestring)
            }
            tot_len <- tot_len + len_seg
        }
    }
    return(sample_lines)
}
## ---- greyscale
# combine three band rgb
greyscale <- function(x) {
    x <- (x[[1]] + x[[2]] + x[[3]]) / 3
}

## ---- lm_compute
# function to compute individual linear models per
# sample
lm_compute <- function(x, f) {
    tryCatch(
        {
            m <- lm(formula = f, data = x)

            # find p vals
            p <- m %>%
                tidy() %>%
                dplyr::select(p = p.value)

            pred_m <- predict(m, x, type = "response")

            # remove average p val above 0.05
            if (sum(p) / nrow(p) < 0.05) {
                x$lm <- pred_m
            } else {
                x$lm <- NA
            }

            # find 95th quantiles
            x$I_dum <- ifelse(x$lm > quantile(x$lm, .95), 1, 0)

            return(x)
        },
        error = function(e) NULL
    )
}

## ---- filter_returns
# remove samples with any road points with a return above 1
filter_returns <- function(x) {
    road <- x[x$road == 1, ]
    if (max(road$NumberOfReturns) == 1) {
        return(x)
    }
}

## ---- filter_samples
filter_samples <- function(s) {
    # find rows with fewer than 8 samples
    # 8 chosen as ~2m^2 given 25cm res
    if (nrow(s) > 8) {
        # remove outlier points
        # distance based isolation filtering
        distances <- s %>%
            st_distance() %>%
            apply(1, FUN = function(y) {
                min(y[y > 0])
            }) %>%
            as.data.frame() %>%
            mutate(rowid = row_number()) %>%
            select(min_dist = ".", rowid)

        # given min dist between two points
        # remove any above 1m from any other point
        distances <- distances[distances$min_dist < 1, ]

        s <- s %>% mutate(rowid = row_number())

        # remove excluded index values
        s <- s[s$rowid %in% distances$rowid, ]
        return(s)
    }
}

## ---- max_dist
# two furthest points in a sample
# convert to a linestring to assume max detected road points
max_dist <- function(x) {
    tot_dists <- c()
    # gives largest distances for a collection of pts
    distances <- x %>%
        st_distance(by_element = FALSE) %>%
        unclass() %>%
        "[<-"(lower.tri(., diag = TRUE), NA) %>%
        as_tibble() %>%
        rowid_to_column() %>%
        gather(colid, distance, starts_with("V"),
            na.rm = TRUE
        ) %>%
        arrange(desc(distance))

    # use colid to find index of pts with largest distances
    if (nrow(distances) > 0) {
        distances$colid <- gsub("[^0-9.-]", "", distances$colid)
        tot_dists <- rbind(tot_dists, max(distances$distance))

        distances <- as.list(distances[1, 1:2]) %>%
            unlist() %>%
            as.numeric()

        # convert two pts to linestring
        x <- x[distances, ] %>%
            st_combine() %>%
            st_sf() %>%
            st_cast("LINESTRING")
        return(x)
    }
}

## ---- max_lines
# combines points filtering and max dist linestrings
# adds linestring length for later
max_lines <- function(x, cents) {
    road_lm <- split(x, f = x$sample_id)

    road_lm <- road_lm %>% compact()

    # filter samples with few points and isolated points  >1m
    road_lm <- lapply(road_lm, filter_samples)
    road_lm <- road_lm %>% compact()
    # create linestrings
    road_lm <- lapply(road_lm, max_dist)
    road_lm <- do.call(rbind, road_lm)
    road_lm$length <- as.numeric(st_length(road_lm))
    # find intersecting buffers, ensure intersects centreline
    # prevents lines taller than wide
    road_lm <- st_join(road_lm, cents)

    return(road_lm)
}

## ---- mid_pts
# find mid point between linestring
mid_pts <- function(x) {
    fixed_cents <- st_coordinates(x)[, 1:2]
    x_mid <- mean(fixed_cents[, 1])
    y_mid <- mean(fixed_cents[, 2])
    mid_point <- cbind(x_mid, y_mid)
    mid_point <- as.data.frame(mid_point)
    mid_point <- mid_point %>%
        st_as_sf(coords = c("x_mid", "y_mid"), crs = 27700)
    return(mid_point)
}

## ---- true_cents
# using mid points convert a list of mid points into
# linestring, i.e. new road centreline
true_cents <- function(x) {
    rd <- unique(x$road_id)
    y <- x %>%
        distinct()
    n <- nrow(y) - 1
    if (nrow(y) > 2) {
        y <- lapply(X = 1:n, FUN = function(i) {
            pair <- y[c(i, i + 1), ] %>%
                st_combine()
            line <- st_cast(pair, "LINESTRING")
            return(line)
        })
        y <- do.call(c, y)
        # remove some noise through filtering out v large lines
        # optimal was qualitatively assessed
        y <- y[as.numeric(st_length(y)) <
            sum(as.numeric(st_length(y))) / (length(y) / 4)]

        y <- y %>%
            st_combine() %>%
            st_cast("MULTILINESTRING")
        y <- y %>% st_sf()

        y <- y[is.na(rd)]

        y$road_id <- as.character(rd)
        return(y)
    }
}

## ---- adjacent_length
# use atan2 to find true width of roads given
# a non perpendicular line, convert to perpendicular to find width
adjacent_length <- function(samp, cent) {
    tot_width <- c()
    cent <- cent %>% st_cast("POINT")
    n <- nrow(cent) - 1
    nodelines <- lapply(X = 1:n, FUN = function(i) {
        pair <- cent[c(i, i + 1), ] %>%
            st_combine()
        line <- st_cast(pair, "LINESTRING")
        return(line)
    })

    samp <- samp %>%
        mutate(row_id = row_number())
    samp <- split(samp, samp$row_id)

    for (n in nodelines) {
        for (s in samp) {
            # find which centreline it is associated with
            # as road consist of multiple
            int <- as.numeric(st_crosses(n, s))
            int[is.na(int)] <- 0
            # with correct line, find perpendicular angle
            # and length
            if (int == 1) {
                n1 <- st_coordinates(n)[1, ]
                n2 <- st_coordinates(n)[2, ]
                x <- n1[1] - n2[1]
                y <- n1[2] - n2[2]
                ang_rad <- atan2(y, x)
                ang_deg <- ang_rad * 180 / pi

                n1 <- st_coordinates(s)[1, ]
                n2 <- st_coordinates(s)[2, ]
                x <- n1[1] - n2[1]
                y <- n1[2] - n2[2]

                ang_rad <- atan2(y, x)
                ang_deg_c <- ang_rad * 180 / pi

                theta <- abs(ang_deg) - abs(ang_deg_c)

                c1_len <- st_length(s)
                # pythagoras to find adjacent line length
                # left of N same as right of N
                # same as + 2pi
                adjacent <- abs(as.numeric(c1_len) * cos(as.numeric(theta)))
                adjacent <- cbind(
                    adjacent, as.character(unique(cent$road_id)),
                    as.character(unique(cent$sample_id))
                )
                tot_width <- rbind(tot_width, adjacent)
            }
        }
    }
    return(tot_width)
}

## ---- model_comparison
# find estimated mean widths per road
# remove noise given no road above 8m and below 2m
model_comparison <- function(model) {
    road_lm <- model[!is.na(model$road_id), ]
    rds <- unique(model$road_id)
    road_lm <- split(road_lm, f = road_lm$road_id)

    samp <- Filter(function(x) dim(x)[1] > 0, road_lm)
    cent <- centrelines[centrelines$road_id %in% rds, ]
    cent <- split(cent, f = cent$road_id)
    cent <- Filter(function(x) dim(x)[1] > 0, cent)

    widths <- mapply(adjacent_length, samp, cent)
    widths <- do.call(rbind, widths)
    widths <- as.data.frame(widths)

    widths$adjacent <- as.numeric(unfactor(widths$adjacent))

    widths <- widths[widths$adjacent > 2 & widths$adjacent < 8, ]

    widths <- widths %>%
        group_by(V2) %>%
        select(road_id = V2, adjacent) %>%
        summarise(
            mean_width = mean(adjacent)
        )

    return(widths)
}

## ---- road_angles
# atan2 to find angle between two centreline segments
# relative to previous centreline orientation
road_angles <- function(rd) {
    coords <- rd %>% st_coordinates()
    angle <- c()
    if (nrow(coords) > 1) {
        for (i in 1:(nrow(rd) - 1)) {
            n1 <- coords[i, ]
            n2 <- coords[i + 1, ]
            x <- n1[1] - n2[1]
            y <- n1[2] - n2[2]
            ang_rad <- atan2(y, x)
            ang_deg <- ang_rad / pi * 180

            angle <- append(angle, ang_deg)
            # left of N same as right of N
            # same as + 2pi
            angle <- abs(angle)
        }
    }

    # normalise angle, i.e. use prev orientation to find true difference in angle
    normal_ang <- c()
    for (i in 2:length(angle)) {
        # here i - 1 is theta 1, i is theta 2
        normal <- abs(angle[i] - (angle[i - 1]))
        normal_ang <- rbind(normal_ang, normal)
    }
    normal_ang <- cbind(
        normal_ang,
        as.character(rep(unique(rd$road_id), nrow(normal_ang)))
    )
    return(normal_ang)
}

## ---- height_change
# find difference in average height between two samples
height_change <- function(x) {
    elev <- c()
    samples <- split(x, x$sample_id)
    if (length(samples) > 2) {
        for (s in 2:length(samples) - 1) {
            pair <- samples[c(s, s + 1)]
            n1 <- mean(pair[[1]]$Z)
            n2 <- mean(pair[[2]]$Z)
            e <- abs(n1 - n2)
            e <- cbind(
                as.character(unique(samples[[s]]$road_id)), e
            )
            elev <- rbind(elev, e)
        }
    }
    return(elev)
}

## ---- surface_qual
# find difference in average height between two samples
surface_qual <- function(x) {
    elev <- c()
    samples <- split(x, x$sample_id)
    if (length(samples) > 2) {
        for (s in 2:length(samples) - 1) {
            pair <- samples[c(s, s + 1)]
            n1 <- mean(pair[[1]]$Z)
            n2 <- mean(pair[[2]]$Z)
            e <- abs(n1 - n2)
            e <- cbind(
                as.character(unique(samples[[s]]$road_id)), e
            )
            elev <- rbind(elev, e)
        }
    }
    return(elev)
}

## ---- printList
printList <- function(x, out.format = knitr::opts_knit$get("out.format"),
                      environment = "itemize",
                      marker = NULL) {
    if (out.format == "markdown") {
        if (!missing(environment) || !missing(marker)) {
            warning("Ignoring arguments that are not supported for markdown output.")
        }
        out <- sprintf("\n\n%s\n \n", paste("*", x, collapse = "\n"))
    } else {
        if (out.format == "latex") {
            itemCommand <- if (missing(marker)) {
                "\\item"
            } else {
                sprintf("\\item[%s]", marker)
            }
            listEnv <- c(
                sprintf("\\begin{%s}\n", environment),
                sprintf("\n\\end{%s}\n", environment)
            )
            out <- paste(itemCommand, x, collapse = "\n")
            out <- sprintf("%s%s%s", listEnv[1], out, listEnv[2])
        } else {
            stop("Output format not supported.")
        }
    }
    return(knitr::asis_output(out))
}

## ---- lm_beta
lm_beta <- function(model) {
    b <- summary(model)$coef[-1, 1]
    sx <- apply(model$model[-1], 2, sd)
    sy <- apply(model$model[1], 2, sd)
    beta <- b * sx / sy
    return(beta)
}
