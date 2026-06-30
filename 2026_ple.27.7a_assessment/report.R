## Prepare plots and tables for report


library(icesTAF)
library(stockassessment)

mkdir("report")

sourceTAF("parameters.R")

sourceTAF("report_plots.R")

sourceTAF("report_tables.R")

sourceTAF("report_doc.R")

if (file.exists("report_sg.R")) {
  sourceTAF("report_sg.R")
} else {
  cat("File does not exist, skipping this line.\n")
}

sourceTAF("sessionInfo.R")

