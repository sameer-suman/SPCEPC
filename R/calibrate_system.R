#' Calibrate EPC-SPC Parameters from Historical Data
#'
#' @param hist_data Numeric vector of historical, in-control process data
#' @param target The target value for the process (default 0)
#' @param lambda EWMA smoothing parameter (default 0.1)
#' @param cusum_k_mult Multiplier for CUSUM allowance k (default 0.5)
#' @param cusum_h_mult Multiplier for CUSUM limit h (default 5)
#' @return A list containing tuned parameters and control limits
#' @export
calibrate_system <- function(hist_data, target = 0, lambda = 0.1,
                             cusum_k_mult = 0.5, cusum_h_mult = 5) {

  y_centered <- hist_data - target

  model <- suppressWarnings(stats::arima(y_centered, order = c(1, 0, 1), include.mean = FALSE))

  phi_est <- as.numeric(model$coef["ar1"])
  theta_est <- as.numeric(-model$coef["ma1"])

  sigma_e <- sqrt(model$sigma2)
  sigma_hat <- sigma_e * sqrt(1 + (phi_est - theta_est)^2)

  shew_ucl <- target + (3 * sigma_hat)
  shew_lcl <- target - (3 * sigma_hat)
  ewma_ucl <- target + (3 * sigma_hat * sqrt(lambda / (2 - lambda)))
  ewma_lcl <- target - (3 * sigma_hat * sqrt(lambda / (2 - lambda)))

  # Calculate absolute CUSUM parameters based on the machine's true variance
  cusum_k <- cusum_k_mult * sigma_hat
  cusum_h <- cusum_h_mult * sigma_hat

  return(list(
    Target = target,
    Phi = phi_est,
    Theta = theta_est,
    Sigma = sigma_hat,
    Shewhart_Limits = c(LCL = shew_lcl, UCL = shew_ucl),
    EWMA_Limits = c(LCL = ewma_lcl, UCL = ewma_ucl),
    CUSUM_Limits = c(k = cusum_k, h = cusum_h)
  ))
}
