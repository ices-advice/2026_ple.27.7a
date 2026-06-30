#'##############################################################################
#' get information on the session and packages loaded                          #
#'##############################################################################

#sinks the data into connection as text file
sink("report/Session_info.txt")

info <- sessionInfo()
info$loadedOnly <- c()
print(info)

sink()
