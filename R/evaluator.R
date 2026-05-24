#' Evaluate SPC-EPC System Performance
#'
#' @param iterations Number of simulation runs (default 100)
#' @param n Observations per run (default 500)
#' @param shift_time Time the assignable cause occurs (default 251)
#' @param disturbance_type "shift" or "trend"
#' @param shift_mag Magnitude of the shift
#' @param trend_mag Magnitude of the trend
#' @param spc_type "shewhart", "ewma", or "cusum"
#' @param intervention Logical: Does the operator fix the process?
#' @return A list containing average PM and average ARL
#' @export
evaluate_system <- function(iterations = 100, n = 500, shift_time = 251,
                            disturbance_type = "shift", shift_mag = 10, trend_mag = 0.1,
                            spc_type = "shewhart", intervention = TRUE) {

  pm_results <- numeric(iterations)
  rl_results <- numeric(iterations)

  for (i in 1:iterations) {
    sim_data <- simulate_epc_spc(n = n, shift_time = shift_time,
                                 disturbance_type = disturbance_type,
                                 shift_mag = shift_mag, trend_mag = trend_mag,
                                 spc_type = spc_type, intervention = intervention)

    pm_results[i] <- mean((sim_data$Output)^2)

    post_shift_signals <- which(sim_data$Signal == TRUE & sim_data$Time >= shift_time)

    if (length(post_shift_signals) > 0) {
      rl_results[i] <- post_shift_signals[1] - shift_time + 1
    } else {
      rl_results[i] <- n - shift_time + 1
    }
  }

  return(list(
    Iterations = iterations,
    Disturbance = disturbance_type,
    spc_type = spc_type,
    Average_PM = mean(pm_results),
    Average_ARL = mean(rl_results)
  ))
}
