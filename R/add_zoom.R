#' add_zoom: Zoom in on region of another function.
#' Allows you to easily change an existing function
#' so that [0,1]^n refers to a subregion of the original function
#'
#' @param func Function to add linear terms to
#' @param scale_low Vector of low end of scale values for each dimension
#' @param scale_high Vector of high end of scale values for each dimension
#'
#' @return Function with added linear terms
#' @export
#'
#' @examples
#' banana(c(.5,.85))
#' add_zoom(banana, c(0,.5), c(1,1))(c(.5,.7))
#' cf::cf(banana)
#' cf::cf(add_zoom(banana, c(0,.5), c(1,1)))
#' cf::cf(add_zoom(banana, c(.2,.5), c(.8,1)))
add_zoom <- function(func, scale_low, scale_high) {
  function(X, ...) {
    if (is.matrix(X)) {
      X_scaled <- sweep(X=X, MARGIN=2,
                        STATS=scale_high-scale_low,
                        FUN = '*') #x * (scale_high - scale_low) + scale_low
      X_scaled <- sweep(X=X_scaled, MARGIN=2,
                        STATS=scale_low)
    } else {
      X_scaled <- X * (scale_high - scale_low) + scale_low
    }
    func(X_scaled)
  }
}