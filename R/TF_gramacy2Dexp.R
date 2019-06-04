#' gramacy2Dexp: gramacy2Dexp function
#' 2 dimensional function.
#' From Gramacy and Lee (2009).
#' @export
#'
#' @rdname test_func_apply
#' @references Gramacy, Robert B., and Herbert KH Lee.
#' "Adaptive design and analysis of supercomputer experiments."
#' Technometrics 51.2 (2009): 130-145.
#' @examples
#' gramacy2Dexp(runif(2))
#' gramacy2Dexp(matrix(runif(2*20),ncol=2))
gramacy2Dexp <- function(x, scale_it=T, scale_low = -2, scale_high = 6, noise=0, ...) {
  test_func_apply(func=TF_gramacy2Dexp, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_gramacy2Dexp: gramacy2Dexp function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#' @references Gramacy, Robert B., and Herbert KH Lee.
#' "Adaptive design and analysis of supercomputer experiments."
#' Technometrics 51.2 (2009): 130-145.
#'
#' @examples
#' TF_gramacy2Dexp(rep(0,2))
#' TF_gramacy2Dexp(rep(1,2))
TF_gramacy2Dexp <- function(x) {
  x[1] * exp(-sum(x^2))
}
