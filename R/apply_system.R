#' Apply Integrated EPC-SPC to Real Data
#'
#' @param real_data Numeric vector of actual, uncontrolled process readings
#' @param calibration A list object returned by `calibrate_system()`
#' @param spc_type Type of chart: "shewhart", "ewma", or "cusum" (default "ewma")
#' @param lambda EWMA weight parameter (default 0.1)
#' @return A data frame of recommended adjustments and SPC signals
#' @export
apply_system <- function(real_data, calibration, spc_type = "ewma", lambda = 0.1) {

  n <- length(real_data)
  target <- calibration$Target
  phi <- calibration$Phi
  theta <- calibration$Theta

  Y_adj <- numeric(n)
  u <- numeric(n)
  Z <- numeric(n)
  Cp <- numeric(n)          # CUSUM positive drift
  Cn <- numeric(n)          # CUSUM negative drift
  spc_signal <- logical(n)

  Y_adj[1] <- real_data[1] - target
  Z[1] <- lambda * Y_adj[1]

  if (spc_type == "cusum") {
    k <- calibration$CUSUM_Limits["k"]
    h <- calibration$CUSUM_Limits["h"]
    Cp[1] <- max(0, Y_adj[1] - k)
    Cn[1] <- max(0, -Y_adj[1] - k)
  } else {
    Cp[1] <- 0
    Cn[1] <- 0
  }

  u[1] <- -(phi - theta) * Y_adj[1]
  spc_signal[1] <- FALSE

  if (spc_type == "shewhart") {
    ucl <- calibration$Shewhart_Limits["UCL"]
    lcl <- calibration$Shewhart_Limits["LCL"]
  } else if (spc_type == "ewma") {
    ucl <- calibration$EWMA_Limits["UCL"]
    lcl <- calibration$EWMA_Limits["LCL"]
  } else if (spc_type == "cusum") {
    ucl <- calibration$CUSUM_Limits["h"]
    lcl <- 0 # CUSUM only crosses upper threshold (h)
  }

  for (t in 2:n) {
    Y_adj[t] <- (real_data[t] - target) + u[t-1]
    u[t] <- (phi * u[t-1]) - ((phi - theta) * Y_adj[t])

    if (spc_type == "shewhart") {
      spc_signal[t] <- ifelse((Y_adj[t] + target) > ucl | (Y_adj[t] + target) < lcl, TRUE, FALSE)
    } else if (spc_type == "ewma") {
      Z[t] <- lambda * Y_adj[t] + (1 - lambda) * Z[t-1]
      spc_signal[t] <- ifelse((Z[t] + target) > ucl | (Z[t] + target) < lcl, TRUE, FALSE)
    } else if (spc_type == "cusum") {
      Cp[t] <- max(0, Y_adj[t] - k + Cp[t-1])
      Cn[t] <- max(0, -Y_adj[t] - k + Cn[t-1])
      spc_signal[t] <- ifelse(Cp[t] > ucl | Cn[t] > ucl, TRUE, FALSE)
    }
  }

  return(data.frame(
    Time = 1:n,
    Raw_Reading = real_data,
    Adjusted_Output = Y_adj + target,
    Recommended_Action = u,
    SPC_Statistic = if(spc_type == "shewhart") Y_adj + target else if(spc_type == "ewma") Z + target else NA,
    CUSUM_P = Cp,
    CUSUM_N = Cn,
    UCL = ucl,
    LCL = lcl,
    Signal = spc_signal
  ))
}
