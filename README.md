
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
The linear program is solved using the
[ROI](https://cran.r-project.org/package=ROI) package that offeres a
unified interface for a variety of
[solvers](https://cran.r-project.org/web/views/Optimization.html). A
solver is a program (package) that can solve those linear programs.

I recommend using `glpk` or `lpSolve` as the original implementation.
But you can also use your favorite commerical solver. However large
linear programs could still be difficult to solve. The complexity is
probably mainly determined by the design matrix (i.e. \#cols, \#rows,
sparsity).

## Installation

~~You can install the released version of losep from~~
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("losep")
```

Or from Github:

``` r
remotes::install_github("dirkschumacher/losep")
```

## What can it do for me?

`losep` can help you detect separation before bad things happen.

As part of your analysis, just add `assert_no_separation` after the
model fit to test for separation.

``` r
...
 # make sure at least one solver is loaded that can handle linear programs
library(ROI.plugin.glpk)
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
# uses any compatible loaded solver
tryCatch(assert_no_separation(model), error = print)
#> <simpleError: Separation detected in your model among following variables:
#> x3>

# or solve it using GLPK with the option presolve
try(assert_no_separation(model, solver = "glpk", presolve = TRUE))
#> Error : Separation detected in your model among following variables:
#> x3
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
#>  15.428   4.741  20.740
```

``` r
system.time(
  tryCatch(assert_no_separation(model), error = print)
)
#> <simpleError: Separation detected in your model among following variables:
#> (Intercept), x>
#>    user  system elapsed 
#>   6.119   1.503   7.909
```

And with verbose output and an additional solver option (presolve):

``` r
system.time(
  try(
    assert_no_separation(model,
      solver = "glpk",
      presolve = TRUE,
      verbose = TRUE
    )
  )
)
#> <SOLVER MSG>  ----
#> GLPK Simplex Optimizer, v4.65
#> 1000000 rows, 8 columns, 7499219 non-zeros
#> Preprocessing...
#> 1000000 rows, 8 columns, 7499219 non-zeros
#> Scaling...
#>  A: min|aij| =  2.770e-08  max|aij| =  5.080e+00  ratio =  1.834e+08
#> GM: min|aij| =  3.447e-04  max|aij| =  2.901e+03  ratio =  8.418e+06
#> EQ: min|aij| =  1.188e-07  max|aij| =  1.000e+00  ratio =  8.418e+06
#> Constructing initial basis...
#> Size of triangular part is 1000000
#>       0: obj =  -4.988989672e+05 inf =   9.205e+05 (567126)
#>      17: obj =   2.075943591e-09 inf =   2.766e-09 (0)
#> *    59: obj =   5.007810000e+05 inf =   2.947e-09 (0)
#> OPTIMAL LP SOLUTION FOUND
#> <!SOLVER MSG> ----
#> Error : Separation detected in your model among following variables:
#> (Intercept), x
#>    user  system elapsed 
#>  12.746   2.315  15.707
```

## Contribution and lifecycle

If you find any bugs or have further ideas, please let me know. Either
by writing an issue, sending a PR or an email. Before you send a PR,
please post an issue first.

## Why losep?

losep = logistic regression, separation

## References and related work

1: Kjell Konis (2007). Linear programming algorithms for detecting
separated data in binary logistic regression models. Ph. D. thesis,
University of Oxford.

The author of the method implemented the algorithm in the very useful
package
[safeBinaryRegression](https://cran.r-project.org/package=safeBinaryRegression).
The package overloads the `glm` function and adds a test for separation
to it. This package aims at decoupling the test from `glm` and
potentially to use it with other inputs as well.
