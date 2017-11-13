#' name123: name123 function
#' ? dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' name123(runif(?))
#' name123(matrix(runif(?*20),ncol=?))
name123 <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_name123, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_name123: name123 function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_name123(rep(0,?))
#' TF_name123(rep(1,?))
TF_name123 <- function(x) {

}
