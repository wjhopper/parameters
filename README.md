# The Parameters Pacakge
Implements a simple S4 class for parameters used in model fitting.
  
Parameter objects are instantiated with their values, names and upper and lower bounds on their possible values. These objects are useful when optimizing a function using unconstrained, derivative free methods (e.g., Nelder-Mead).

## Installation
This package is not on CRAN, but can be easily installed directly from this repository using the `devtools` package

```r
install.packages("devtools") # Install the devtools package if not already installed
devtools::install_github("wjhopper/parameters")
library(parameters)
```
