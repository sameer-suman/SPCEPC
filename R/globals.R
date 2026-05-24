#' @importFrom rlang .data
NULL

# Prevent R CMD check from complaining about ggplot2 column names
utils::globalVariables(c("Time", "Output", "Signal", "EWMA", "CUSUM_P", "CUSUM_N"))
