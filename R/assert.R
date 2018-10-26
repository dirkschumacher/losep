#' Assert no (quasi) separation binary classifcation model
#'
#' @param model the classiciation model
#' @param solver a length 1 character vector to control which solvers is used
#' to solve the underlying linear program. Please see the \code{ROI} package
#' for a list of available solvers.
#' @param ... passed as parameters to \code{ROI_solve}
#'
#' The function throws an error if separation is detected. Otherwise it returns
#' false invisibly.
#'
#' The function formulates a linear programming (LP) model and solves it using
#' the \code{ROI} package. The \code{ROI} package offers a unified interface
#' towards a range of linear programming solvers (i.e. specialized packages to
#' solve LPs.).
#'
#'
#' @examples
#' \dontrun{
#' library(ROI.plugin.glpk)
#' data <- data.frame(
#'   x = factor(c(1, 1, 1, 2, 2, 2, 3, 3)),
#'   y = c(1, 1, 0, 1, 1, 0, 1, 1)
#' )
#'
#' model <- glm(y ~ -1 + x, data = data, family = "binomial")
#'
#' # throws an error if the data is separable
#' assert_no_separation(model) #uses any compatible loaded solver
#'
#' # or solve it using GLPK with the option presolve
#' assert_no_separation(model, solver = "glpk", presolve = TRUE)
#' }
#' @references
#' Konis, K. (2007).
#' Linear programming algorithms for detecting separated data in binary logistic
#' regression models. Ph. D. thesis, University of Oxford.
#' @export
assert_no_separation <- function(model, solver, ...) {
  UseMethod("assert_no_separation")
}

#' @export
assert_no_separation.glm <- function(model, solver = "auto", ...) {
  if (!identical(model$family$family, "binomial")) {
    warning("Only Generalized Linear Models with family 'binomial'",
            " are supported",
      call. = FALSE
    )
    return(TRUE)
  }

  response <- stats::model.response(model$model, "any")

  # just to be safe
  unique_classes <- unique(response)
  stopifnot(
    is.numeric(unique_classes),
    length(unique_classes) == 2L,
    all(unique_classes %in% c(0, 1))
  )
  X <- stats::model.matrix(model$terms, model$model, model$contrasts)

  # the model here is based on Konis (2007), chapter 4. In particular
  # sections 4.2, 4.4.3 and 4.4.4

  # build the ROI model

  # transform the model matrix so that all constraints are >=
  # that should also work with floats
  response[response == 0] <- -1
  X_bar <- X * response

  m <- ncol(X_bar)
  n <- nrow(X_bar)

  constraints <- ROI::L_constraint(X_bar, rep.int(">=", n), rep.int(0, n))

  bounds <- ROI::V_bound(
    li = seq_len(m), lb = rep.int(-1, m),
    ui = seq_len(m), ub = rep.int(1, m)
  )

  # max t(rep.int(1, n)) %*% X_bar %*% beta = colSums(X_bar) %*% beta
  # subject to X_bar >= 0
  # beta between -1 and 1
  opt_model <- ROI::OP(colSums(X_bar),
    constraints,
    bounds = bounds,
    maximum = TRUE
  )

  available_solvers <- ROI::ROI_applicable_solvers(opt_model)
  if (!is.character(available_solvers) && length(available_solvers) == 0L) {
    stop(
      "No ROI solver plugin loaded for linear programs. ",
      "We recommend using ROI.plugin.glpk. ",
      "Simply type library(ROI.plugin.gpk)."
    )
  }

  # if the LP is unbounded, seperation exists. Otherwise an optiomal solution
  # with obj. value 0 exists.
  result <- ROI::ROI_solve(opt_model, solver = solver, ...)

  # an optimal solution should always exists
  stopifnot(identical(as.integer(result$status$code), 0L))

  # compare to 0 zero with tolerance
  non_zero <- abs(result$solution) > sqrt(.Machine$double.eps)
  has_seperation <- any(non_zero)

  if (has_seperation) {
    stop("Separation detected in your model in the following variables:\n",
      paste0(names(result$solution)[non_zero], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
