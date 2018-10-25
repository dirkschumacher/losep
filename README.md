
<!-- README.md is generated from README.Rmd. Please edit that file -->

# losep

[![Travis build
status](https://travis-ci.org/dirkschumacher/losep.svg?branch=master)](https://travis-ci.org/dirkschumacher/losep)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/losep?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/losep)
[![Coverage
status](https://codecov.io/gh/dirkschumacher/losep/branch/master/graph/badge.svg)](https://codecov.io/github/dirkschumacher/losep?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/losep)](https://cran.r-project.org/package=losep)

Detect linear separation in binary classification problems using linear
programming.

To cite the author of this method \[1\]:

> The parameter estimates of a binary logistic regression model fit
> using the method of maximum likelihood sometimes do not converge to
> finite values. This phenomenon (also known as monotone likelihood or
> infinite parameters) occurs because of a condition among the sample
> points known as separation. There are two classes of separation.  
> When complete separation is present among the sample points, iterative
> procedures for maximizing the likelihood tend to break down, when it
> would be clear that there is a problem with the model. However, when
> quasicomplete separation is present among the sample points, the
> iterative procedures for maximizing the likelihood tend to satisfy
> their convergence criterion before revealing any indication of
> separation.

The package exports a single assertion function to test for separation.
The linear program is solved using the `ROI` package that offeres a
unified interface for a variety of solvers. A solver is a program
(package) that can solve those linear programs.

I recommend using `glpk` or `lpSolve` as the original implementation.
But you can also use your favorite commerical solver.

## Installation

You ~~can install~~ the released version of losep from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("losep")
```

``` r
remotes::install_github("dirkschumacher/losep")
```

## What can it do for me?

`losep` can help you detect separation before bad things happen.

As part of your analysis, just add `assert_no_separation` after the
model fit to test for separation.

``` r
...
my_model <- glm(formula, data, family = "binomial")
assert_no_separation(my_model) # this uses a default solver (works in most cases)
...
```

Or with small toy example. In this case, for `x=3`, the response is
always 1.

``` r
library(losep)
library(ROI.plugin.glpk)
data <- data.frame(
  x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
  y = c(1, 1, 0, 1, 1, 0, 1, 1)
)

model <- glm(y ~ -1 + x, data = data, family = "binomial")

# throws an error if the data is seperable
tryCatch(assert_no_separation(model), error = print) #uses any compatible loaded solver
#> <simpleError: Seperation detected in your model in the following variables:
#> x3>

# or solve it using GLPK with the option presolve
try(assert_no_separation(model, solver = "glpk", presolve = TRUE))
```

## What is the overhead?

According to Konis (2007): not a lot. The linear program involves no
integer variables and should be feasible for fairly large models.

Here is a test with 10^6 observations. Fitting the glm takes longer than
solving the LP with glpk.

``` r
library(losep)
library(ROI.plugin.glpk)
n <- 1e6
data <- tibble::tibble(
  x = as.integer(runif(n) < 0.5),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n),
  x7 = rnorm(n),
  y = x
)
system.time(
  model <- glm(y ~ 1 + x + x2 + x3 + x4 + x5 + x6 + x7,
               data = data, 
               family = "binomial")
)
#> Warning: glm.fit: algorithm did not converge
#>    user  system elapsed 
#>  14.232   3.544  18.045
```

``` r
system.time(
  tryCatch(assert_no_separation(model), error = print)
)
#> <simpleError: Seperation detected in your model in the following variables:
#> (Intercept), x>
#>    user  system elapsed 
#>   6.293   1.145   7.678
```

And with verbose output:

``` r
system.time(
  try(
    assert_no_separation(model,
      solver = "glpk",
      verbose = TRUE
    )
  )
)
#> <SOLVER MSG>  ----
#> GLPK Simplex Optimizer, v4.63
#> 1000000 rows, 8 columns, 7498984 non-zeros
#>       0: obj =  -4.954764365e+05 inf =   1.419e+06 (566470)
#>      10: obj =   9.473118290e-11 inf =   2.021e-10 (0)
#> *    44: obj =   5.010160000e+05 inf =   1.157e-09 (0)
#> OPTIMAL LP SOLUTION FOUND
#> <!SOLVER MSG> ----
#>    user  system elapsed 
#>   7.055   1.368   8.903
```

## Contribution and lifecycle

This is the very first version. If you find any bugs or have further
ideas, please let me know. Either by writing an issue, sending a PR or
an email. Before you send a PR, please post an issue first.

## Why losep?

losep = logistic regression, separation

I am not good at naming things.

## References and related work

1: Konis, K. (2007). Linear programming algorithms for detecting
separated data in binary logistic regression models. Ph. D. thesis,
University of Oxford.

The author of the method implemented the algorithm in the package
`safeBinaryRegression`. The package overloads the `glm` function and
adds a test for separation to it. I wanted to decouple the test from
`glm` and potentially use it with other inputs as well.
