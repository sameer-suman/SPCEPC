# spcepc: Predictive Maintenance & Integrated Process Control

**The "Hidden Factory" Problem:** In modern automated manufacturing, machines rely on Engineering Process Control (EPC) to continuously adjust themselves and keep products on target. However, these automated controllers are so effective that they often **mask underlying mechanical failures** (like tool wear, sensor drift, or leaks). The physical parts look perfect, but the machine is straining itself until it catastrophically breaks.

The `spcepc` package bridges the gap between engineering automation and statistical monitoring. Based on the foundational framework by Montgomery et al. (1994), this package allows data scientists to overlay Statistical Process Control (SPC) onto automated systems. By statistically monitoring the machine's *internal effort*, you can catch masked drift before it causes factory downtime.

## Core Features

* **Time-Series Simulation:** Generate realistic ARMA(1,1) manufacturing data with sudden shifts or gradual trends.
* **Automated Controllers:** Simulate Minimum Mean Square Error (MMSE) controllers compensating for process drift.
* **Statistical Dashboards:** Apply Shewhart, EWMA, and CUSUM charts to flag masked anomalies.
* **System Calibration:** Ingest historical industrial data to dynamically calculate specific process parameters ($\phi$, $\theta$) and statistical control limits.
* **Financial Evaluation:** Calculate Performance Measure (Total Variance) to prove the ROI of early intervention.

## Installation

You can install the development version of `spcepc` from GitHub using the `devtools` package:

``` r
# install.packages("devtools")
devtools::install_github("sameer-suman/spcepc")
```

## Quick Start: The Masked Failure

Imagine a machine that experiences a slow, creeping failure (tool wear) starting at observation 100. Because an automated controller is actively fighting the failure, the raw output looks perfectly fine. `spcepc` allows us to apply an **EWMA chart** to the controller's effort to easily catch the failure.

``` r
library(spcepc)
set.seed(123)

# Simulate a 200-period production run with a creeping failure at t=100
production_run <- simulate_epc_spc(
  n = 200, 
  shift_time = 100, 
  disturbance_type = "trend",
  trend_mag = 0.05,
  spc_type = "ewma"
)

# Visualize the hidden failure being caught by the EWMA chart
plot_ewma(production_run)
```

## The Case Study (Vignette)

For a comprehensive tutorial on how to deploy this package—from prototyping the math on a simulated machine to calibrating the system on a live factory floor—please read the included vignette:

```r
vignette("predictive_maintenance", package = "spcepc")
```

## References
Montgomery, D. C., Keats, J. B., Runger, G. C., & Messina, W. S. (1994). *Integrating Statistical Process Control and Engineering Process Control.* Journal of Quality Technology, 26(2), 79-87.
