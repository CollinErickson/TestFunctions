# Just another file with functions

#' logistic: logistic function
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic, from=-5,to=5)
#' curve(logistic(x,offset=.5, scl=15))
#' logistic(matrix(runif(20),ncol=1))
logistic <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic: logistic function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @param offset Amount it should be offset
#' @param scl Scale parameter
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic(0)
#' TF_logistic(1)
TF_logistic <- function(x, offset=0, scl=1) {
  1 / (1 + exp(-scl*(x-offset)))
}

#' logistic15: logistic15 function.
#' Same as logistic() except adjusted to be reasonable from 0 to 1,
#' has a center at 0.5 and scale of 15.
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic15)
#' curve(logistic15(x,offset=.25))
#' logistic15(matrix(runif(20),ncol=1))
logistic15 <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic15, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic15: logistic15 function for evaluating a single point.
#' Same as logistic except adjusted to be reasonable from 0 to 1.
#'
#' @param x Input vector at which to evaluate.
#' @param offset Amount it should be offset
#' @param scl Scale parameter
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic15(0)
#' TF_logistic15(1)
#' curve(Vectorize(TF_logistic15)(x))
TF_logistic15 <- function(x, offset=.5, scl=15) {
  1 / (1 + exp(-scl*(x-offset)))
}

#' logistic_plateau: logistic_plateau function. Sum of two logistics with
#' a plateau in the middle.
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic_plateau(matrix(x,ncol=1)))
#' logistic_plateau(matrix(runif(20),ncol=1))
logistic_plateau <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic_plateau, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic_plateau: logistic_plateau function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic_plateau(0)
#' TF_logistic_plateau(.5)
TF_logistic_plateau <- function(x) {
  logistic(x, offset=.15, scl=15) - logistic(x, offset=.85,scl=15)
}
