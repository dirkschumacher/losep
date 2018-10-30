context("separation")

test_that("it fails if no solver is loaded", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 0, 1, 1, 1, 0, 1, 0)
  )
  model <- glm(y ~ x, data = data, family = "binomial")
  expect_error(result <- assert_no_separation(model), "glpk")
})

library(ROI.plugin.glpk)

test_that("returns true if family is not binomial", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 0, 1, 1, 1, 0, 1, 0)
  )
  model <- glm(y ~ x, data = data, family = "poisson")
  expect_warning(result <- assert_no_separation(model), "binomial")
  expect_true(result)
})

test_that("returns true if no separation", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 0, 1, 1, 1, 0, 1, 0)
  )
  model <- glm(y ~ x, data = data, family = "binomial")
  expect_silent(result <- assert_no_separation(model))
  expect_true(result)
})

test_that("wors with logicals", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 0, 1, 1, 1, 0, 1, 0) == 1
  )
  model <- glm(y ~ x, data = data, family = "binomial")
  expect_silent(result <- assert_no_separation(model))
  expect_true(result)
})

test_that("detects quasi separation", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 1, 0, 1, 1, 0, 1, 1)
  )
  model <- glm(y ~ -1 + x, data = data, family = "binomial")
  expect_error(assert_no_separation(model), "separation|x3")
})

test_that("detects full separation", {
  data <- data.frame(
    x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
    y = c(1, 1, 1, 0, 0, 0, 0, 0)
  )
  model <- glm(y ~ -1 + x, data = data, family = "binomial")
  expect_error(assert_no_separation(model), "separation|x3")
})
