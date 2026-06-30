library(icesTAF)
sourceTAF("../../../parameters.R")
#sam_assessment <- "Ple.7a_2023_SW"

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/data/"
  )

# read dat files from html
files <-
  gsub(
    ".*>(.+)</a>.*",
    "\\1",
    grep("[.]dat", readLines(sam_dir), value = TRUE)
  )

for (file in files) {
  download(paste0(sam_dir, file))
}

