#' Calculate Performance Measure (PM)
#'
#' @param y Numeric vector of process outputs
#' @param target Numeric target value (default 0)
#' @return A numeric scalar representing the PM
#' @export
calc_pm <- function(y, target = 0) {
  mean((y - target)^2)
}
