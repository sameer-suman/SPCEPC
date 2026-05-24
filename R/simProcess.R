#' Simulate Integrated EPC-SPC Process
#'
#' @param n Number of observations (default 500)
#' @param phi AR(1) parameter (default 0.95)
#' @param theta MA(1) parameter for controller (default 0.4)
#' @param shift_time Index where the disturbance starts (default 251)
#' @param disturbance_type "shift" or "trend"
#' @param shift_mag Magnitude of the sudden shift (default 10)
#' @param trend_mag Magnitude of the trend per period (default 0.1)
#' @param spc_type Type of chart: "shewhart", "ewma", or "cusum"
#' @param lambda EWMA weight parameter (default 0.1)
#' @param cusum_k CUSUM allowance parameter (default 0.5)
#' @param cusum_h CUSUM control limit parameter (default 5)
#' @param intervention Logical: should the disturbance be fixed upon detection?
#' @return A data frame containing the process variables over time
#' @export
simulate_epc_spc <- function(n = 500, phi = 0.95, theta = 0.4,
                             shift_time = 251, disturbance_type = "shift",
                             shift_mag = 10, trend_mag = 0.1,
                             spc_type = "shewhart", lambda = 0.1,
                             cusum_k = 0.5, cusum_h = 5,
                             intervention = TRUE) {

  Y <- numeric(n)
  Z <- numeric(n)
  Cp <- numeric(n)
  Cn <- numeric(n)
  u <- numeric(n)
  n_dist <- numeric(n)
  spc_signal <- logical(n)

  e <- stats::rnorm(n, mean = 0, sd = 1)
  a <- stats::rnorm(n, mean = 0, sd = 1)

  n_dist[1] <- a[1]
  Y[1] <- n_dist[1] + e[1]
  Z[1] <- lambda * Y[1]
  Cp[1] <- max(0, Y[1] - cusum_k)
  Cn[1] <- max(0, -Y[1] - cusum_k)
  u[1] <- -(phi - theta) * Y[1]
  spc_signal[1] <- FALSE

  sigma_hat <- sqrt(1 + (phi - theta)^2)

  shew_ucl <- 3 * sigma_hat
  shew_lcl <- -3 * sigma_hat
  ewma_ucl <- 3 * sigma_hat * sqrt(lambda / (2 - lambda))
  ewma_lcl <- -3 * sigma_hat * sqrt(lambda / (2 - lambda))

  is_fixed <- FALSE
  active_dist <- 0

  for (t in 2:n) {

    if (t > shift_time && spc_signal[t-1] == TRUE && intervention == TRUE) {
      is_fixed <- TRUE
    }

    if (is_fixed) {
      active_dist <- 0
    } else if (t >= shift_time) {
      if (disturbance_type == "shift") {
        active_dist <- shift_mag
      } else if (disturbance_type == "trend") {
        active_dist <- trend_mag * (t - shift_time + 1)
      }
    } else {
      active_dist <- 0
    }

    n_dist[t] <- phi * n_dist[t-1] + a[t]
    Y[t] <- u[t-1] + n_dist[t] + e[t] + active_dist
    u[t] <- (phi * u[t-1]) - ((phi - theta) * Y[t])

    if (spc_type == "shewhart") {
      spc_signal[t] <- ifelse(Y[t] > shew_ucl | Y[t] < shew_lcl, TRUE, FALSE)
    } else if (spc_type == "ewma") {
      Z[t] <- lambda * Y[t] + (1 - lambda) * Z[t-1]
      spc_signal[t] <- ifelse(Z[t] > ewma_ucl | Z[t] < ewma_lcl, TRUE, FALSE)
    } else if (spc_type == "cusum") {
      Cp[t] <- max(0, Y[t] - cusum_k + Cp[t-1])
      Cn[t] <- max(0, -Y[t] - cusum_k + Cn[t-1])
      spc_signal[t] <- ifelse(Cp[t] > cusum_h | Cn[t] > cusum_h, TRUE, FALSE)
    }
  }

  return(data.frame(
    Time = 1:n, Output = Y, EWMA = Z, CUSUM_P = Cp, CUSUM_N = Cn,
    ControlAction = u, Signal = spc_signal,
    UCL = if(spc_type == "shewhart") shew_ucl else if (spc_type == "ewma") ewma_ucl else cusum_h,
    LCL = if(spc_type == "shewhart") shew_lcl else if (spc_type == "ewma") ewma_lcl else 0
  ))
}
