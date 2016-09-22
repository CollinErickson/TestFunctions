#' General function for evaluating a test function
#'
#' @param func A function to evaluate
#' @param x Input value
#' @param scale_it Should the data be scaled from [0, 1]^D to
#' [scale_low, scale_high]? This means the input data is confined
#' to be in [0, 1]^D, but the function isn't.
#' @param scale_low Lower bound for each variable
#' @param scale_high Upper bound for each variable
#' @param noise If white noise should be added, specify the
#' standard deviation for normal noise
#' @param ... Additional parameters for func
#' @param pow Power for powsin
#'
#' @return Function values at x
#' @importFrom stats rnorm rexp runif
#' @export
#'
#' @examples
#' x <- matrix(seq(0,1,length.out=10), ncol=1)
#' y <- test_func_apply(sin, x, TRUE, 0, 2*pi, .05)
#' plot(x,y)
#' curve(sin(2*pi*x), col=2, add=TRUE)
test_func_apply <- function(func, x, scale_it, scale_low, scale_high, noise=0, ...) {#browser()
  if (noise < 0 | !is.numeric(noise)) noise <- 0
  if (is.matrix(x)) {
    noise.out <- rnorm(nrow(x), 0, noise)
    #apply(x, 1, test_func_apply, func=func, scale_it=scale_it, scale_low=scale_low, scale_high=scale_high, ...)
    if (scale_it) {
      return(apply(x, 1, function(y){func(y * (scale_high - scale_low) + scale_low)}, ...) + noise.out)
    } else {
      return((apply(x, 1, func, ...)) + noise.out)
    }
  }
  # otherwise is single value
  noise.out <- rnorm(1, 0, noise)
  if (scale_it) {
    #return(func((x - scale_low) / (scale_high - scale_low)))
    return(func(x * (scale_high - scale_low) + scale_low, ...) + noise.out)
  }
  func(x, ...) + noise.out
}

#' Create a standard test function. This makes it easier to create
#' many functions that follow the same template.
#'
#' @param func A function that takes a vector representing a single point.
#' @param scale_it_ Should the function scale the inputs from [0, 1]^D to
#' [scale_low_, scale_high_] by default? This can be overridden when
#' actually giving the output function points to evaluate.
#' @param scale_low_ What is the default lower bound of the data?
#' @param scale_high_ What is the default upper bound of the data?
#' @param noise_ Should noise be added to the function by default?
#' @param ... Parameters passed to func when evaluating points.
#'
#' @return A test function created using the standard_test_func template.
#' @export
#'
#' @examples
#' gaussian1 <- standard_test_func(.gaussian1, scale_it=F, scale_low = c(0,0), scale_high = c(1,1))
#' .gaussian1 <- function(x, center=.5, s2=.01) {
#'   exp(-sum((x-center)^2/2/s2))
#' }
standard_test_func <- function(func, scale_it_=F, scale_low_ = NULL, scale_high_ = NULL, noise_=0, ...) {
  #if (missing(...)) {dots <- NULL}
  #else {dots <- c(...)}
  dots <- c(...)
  function(x, scale_it=scale_it_, scale_low = scale_low_, scale_high = scale_high_, noise=noise_) {
    test_func_apply(func=func, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, dots)
  }
}


#' A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' branin(runif(2))
branin <- function(x, scale_it=T, scale_low = c(-5, 0), scale_high = c(10, 15)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_branin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_branin <- function(x, a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)) {
  a * (x[2] - b * x[1]^2 + cc * x[1] - r)^2 + s * (1 - tt) * cos(x[1]) + s
}


#' A function estimating water flow through a borehole.
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' borehole(runif(8))
borehole <- function(x, scale_it=T,
                     scale_low = c(.05,100,63070,990,63.1,700,1120,9855),
                     scale_high = c(.15,50000,115600,1110,116,820,1680,12045)) {
  test_func_apply(func=TF_borehole, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_borehole <- function(x) {
  # 8 dim, NOT uniform
  # See: http://www.sfu.ca/~ssurjano/borehole.html
  2 * pi * x[3] * (x[4] - x[6]) /
    (log(x[2] / x[1]) *
       (1 + 2 * x[7] * x[3] / log(x[2] / x[1]) * x[1]^2 * x[8]) +
       x[3] / x[5])
}


#' A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' franke(runif(2))
franke <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/franke2d.html
  test_func_apply(func=TF_franke, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_franke <- function(x) {
  0.75 * exp(-(9 * x[1] - 2)^2 / 4 - (9 * x[2] - 2)^2 / 4) +
    0.75 * exp(-(9 * x[1] + 1)^2 / 49 - (9 * x[2] + 1)^2 / 10) +
    0.5 * exp(-(9 * x[1] - 7)^2 / 4 - (9 * x[2] - 3)^2 / 4) +
    -0.2 * exp(-(9 * x[1] - 4)^2 - (9 * x[2] - 7)^2)
}


#' A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' zhou1998(runif(2))
zhou1998 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_zhou1998, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_zhou1998 <- function(x) {
  # Any dim, http://www.sfu.ca/~ssurjano/zhou98.html
  d <- length(x)
  phi1 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 1 / 3))^2))
  phi2 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 2 / 3))^2))
  10^d / 2 * (phi1 + phi2)
}


#' A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' currin1991(runif(2))
currin1991 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/curretal91.html
  test_func_apply(func=TF_currin1991, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_currin1991 <- function(x) {
  4.9 + 21.15 * x[1] - 2.17 * x[2] - 15.88 * x[1]^2 -
    1.38 * x[2]^2 - 5.26 * x[1] * x[2]
}


#' Some function?
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' lim2002(runif(2))
lim2002 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/limetal02pol.html
  test_func_apply(func=TF_lim2002, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_lim2002 <- function(x) {
  9 + 2.5 * x[1] - 17.5 * x[2] + 2.5 * x[1] * x[2] + 19 * x[2]^2 -
    7.5 * x[1]^3 - 2.5 * x[1] * x[2]^2 - 5.5 * x[2]^4 + x[1]^3 * x[2]^2
}


#' A banana shaped function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' banana(runif(2))
banana <- function(x, scale_it=T, scale_low = c(-20,-10), scale_high = c(20,5)) {
  # 2 dim, See Roshan SMED
  test_func_apply(func=TF_banana, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_banana <- function(x){
  exp(-.005*x[1]^2-.5*(x[2]+.03*x[1]^2-3)^2)
}

#' A Gaussian function centered at 0.5.
#' Any dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' gaussian1(runif(2))
gaussian1 <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_gaussian1, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#gaussian1 <- standard_test_func(TF_gaussian1, scale_it=F, scale_low = c(0,0), scale_high = c(1,1))
TF_gaussian1 <- function(x, center=.5, s2=.01) {
  exp(-sum((x-center)^2/2/s2))
}

#' A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' sinumoid(runif(2))
sinumoid <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_sinumoid, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#sinumoid <- standard_test_func(TF_sinumoid, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0)
TF_sinumoid <- function(x){
  sum(sin(2*pi*x*3)) + 20/(1+exp(-80*(x[[1]]-.5)))
}
#' A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' waterfall(runif(2))
waterfall <- sinumoid


#' A square root of a sine function.
#' Any dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' sqrtsin(runif(1))
sqrtsin <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  test_func_apply(func=TF_sqrtsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
TF_sqrtsin <- function(x, freq=2*pi) {
  ss <- sum(sin(freq*x))
  sqrt(abs(ss))*sign(ss)
}


#' A sine function raised to a power keeping its original sign.
#' Any dimensional function.
#' @rdname test_func_apply
#' @export
#'
#' @examples
#' powsin(runif(1))#,pow=2)
powsin <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0, pow=1) {
  test_func_apply(func=TF_powsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, pow=pow)
}
#powsin <- standard_test_func(TF_powsin, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), pow=1)

#' A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param freq Wave frequency
#' @param pow Power to raise wave to.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_powsin(runif(2))
TF_powsin <- function(x, freq=2*pi, pow=.7) {
  ss <- sum(sin(freq*x))
  (abs(ss) ^ pow) * sign(ss)
}

#' OTL Circuit.
#' 6 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' OTL_Circuit(runif(6))
OTL_Circuit <- function(x, scale_it=T, scale_low = c(50,25,0.5,1.2,0.25,50), scale_high = c(150,70,3,2.5,1.2,300)) {
test_func_apply(func=TF_OTL_Circuit, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#OTL_Circuit <- standard_test_func(TF_OTL_Circuit, scale_it=T,
#                                  scale_low = c(50,25,0.5,1.2,0.25,50),
#                                  scale_high = c(150,70,3,2.5,1.2,300))

#' OTL Circuit function for evaluating a single point
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_OTL_Circuit(c(50,25,0.5,1.2,0.25,50))
TF_OTL_Circuit <- function(x) {
  Vb1 <- 12*x[2] / (x[1] + x[2])
  BRc29 <- x[6] * (x[5] + 9) #+ x[3]
  t1 <- (Vb1 + 0.74) * BRc29 / (BRc29 + x[3])
  t2 <- 11.35 * x[3] / (BRc29 + x[3])
  t3 <- .74 * x[3] * BRc29 / ((BRc29 + x[3]) * x[4])
  t1 + t2 + t3
}
