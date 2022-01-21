source("../scripts/functions.r")
roads_split <- st_read("../data/derived/roads/roads_line.gpkg")

roads_split <- roads_split[roads_split$road_id == "road_4", ] %>%
    st_cast("POINT") %>%
    st_coordinates()

x <- c(1, 2, 3, 5)
y <- c(1, 2, 2, 3)

df <- cbind(x, y)
plot(df, asp = 1) # visualise

angle <- c()
if (nrow(df) > 1) {
    for (i in 1:(nrow(df) - 1)) {
        n1 <- df[i, ]
        n2 <- df[i + 1, ]
        x <- n1[1] - n2[1]
        y <- n1[2] - n2[2]
        ang_rad <- atan2(x, y)
        ang_deg <- ang_rad / pi * 180

        if (ang_rad < 0) {
            ang_deg <- ang_deg + 180
        }
        angle <- append(angle, ang_deg)
    }
}

bearing_ang <- c()
normal_ang <- c()
for (i in 2:length(angle)) {
    # here i - 1 is theta 1, i is theta 2
    normal <- abs(angle[i] - (angle[i - 1]))
    normal_ang <- rbind(normal_ang, normal)
}

bearing_ang <- rbind(bearing_ang, normal_ang)

####

x <- c(1, 2, 3, 1)
y <- c(2, 1, 1, 3)
pts <- as.data.frame(cbind(x, y))

x <- c(1, 2, 3, 5)
y <- c(1, 2, 2, 3)

rd <- as.data.frame(cbind(x, y))

library(tidyverse)
library(sf)
ggplot() +
    geom_line(data = rd, aes(x = x, y = y)) +
    geom_point(data = pts, aes(x = x, y = y), colour = "green")

rd <- rd %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_cast("POINT")

tot_width <- c()
rd <- rd %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_cast("POINT")

coord <- st_coordinates(rd)
n <- nrow(rd) - 1
nodelines <- lapply(X = 1:n, FUN = function(i) {
    pair <- rd[c(i, i + 1), ] %>%
        st_combine()
    line <- st_cast(pair, "LINESTRING")
    return(line)
})

samplines <- lapply(X = 1:n, FUN = function(i) {
    pair <- pts[c(i, i + 1), ] %>%
        st_combine()
    line <- st_cast(pair, "LINESTRING")
    return(line)
})

s <- samplines[[2]]
n <- nodelines[[2]]

tot_width <- c()
for (n in nodelines) {
    for (s in samplines) {
        int <- as.numeric(st_crosses(n, s))
        int[is.na(int)] <- 0
        if (int == 1) {
            n1 <- st_coordinates(n)[1,]
            n2 <- st_coordinates(n)[2,]
            x <- n1[1] - n2[1]
            y <- n1[2] - n2[2]
            ang_rad <- atan2(x, y)
            ang_deg <- ang_rad * 180 / pi
            if (ang_rad < 0) {
                ang_deg <- ang_deg + 180
            }

            n1 <- st_coordinates(s)[1,]
            n2 <- st_coordinates(s)[2,]
            x <- n1[1] - n2[2]
            y <- n1[2] - n2[2]

            ang_rad <- atan2(x, y)
            ang_deg_c <- ang_rad * 180 / pi
            if (ang_rad < 0) {
                ang_deg_c <- ang_deg + 180
            }

            theta <- ang_deg - ang_deg_c
            theta <- theta - 45 # position relation to perp line

            c1_len <- st_length(s)
            opposite <- abs(as.numeric(c1_len) * cos(as.numeric(theta)))
            width <- opposite

            tot_width <- rbind(tot_width, width)
        }
    }
}


##### replace
library(tidyverse)
road_id <- c("road_1", "road_2", "road_3", "road_4")
roadFunction <- c("B", "B")
lm1_mean <- c(2,3,4,7)
lm2_mean <- c(5,9,3,2)
lm_estimate <- c(5,5,5,5)
widths <- data.frame(road_id, roadFunction, lm1_mean, lm2_mean, lm_estimate)

normalise_widths <- function(x, e) {
  x <- x / e * 100
  x <- replace(x, x > 100, (100 - (x - 100)))
}


# normalised comparison relative to known
norm_widths <- widths %>%
  mutate_at(vars(3:length(widths)), normalise_widths, e = widths$lm_estimate)
