# timer
start_time <- Sys.time()

# delete old derived data
unlink("../data/derived/*/*", recursive = FALSE)
unlink("../data/final_data/*", recursive = FALSE)
# scripts
source("00_clean.r")
gc()
print("clean done")
source("01_samples.r")
gc()
print("samples done")
source("02_lm.r")
gc()
print("lm done")
source("03_width.r")
gc()
print("width done")
source("04_cents.r")
print("cents done")
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- (as.numeric(time_taken))
names(time_taken) <- c("time_taken")

write.csv(time_taken, "./time_taken.csv")
