#' Test function.
#'
#' branin: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' branin(runif(2))
#' branin(matrix(runif(20), ncol=2))
branin <- function(x, scale_it=T, scale_low = c(-5, 0), scale_high = c(10, 15), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_branin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' Base test function.
#'
#' TF_branin: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @param a Parameter for TF_branin
#' @param b Parameter for TF_branin
#' @param cc Parameter for TF_branin
#' @param r Parameter for TF_branin
#' @param s Parameter for TF_branin
#' @param tt Parameter for TF_branin
#' @examples
#' TF_branin(runif(2))
TF_branin <- function(x, a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)) {
  a * (x[2] - b * x[1]^2 + cc * x[1] - r)^2 + s * (1 - tt) * cos(x[1]) + s
}


#' borehole: A function estimating water flow through a borehole.
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' borehole(runif(8))
#' borehole(matrix(runif(80), ncol=8))
borehole <- function(x, scale_it=T,
                     scale_low = c(.05,100,63070,990,63.1,700,1120,9855),
                     scale_high = c(.15,50000,115600,1110,116,820,1680,12045), noise=0) {
  test_func_apply(func=TF_borehole, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_borehole: A function taking in a single vector.
#' 8 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_borehole(runif(8))
TF_borehole <- function(x) {
  # 8 dim, NOT uniform
  # See: http://www.sfu.ca/~ssurjano/borehole.html
  2 * pi * x[3] * (x[4] - x[6]) /
    (log(x[2] / x[1]) *
       (1 + 2 * x[7] * x[3] / log(x[2] / x[1]) * x[1]^2 * x[8]) +
       x[3] / x[5])
}


#' franke: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' franke(runif(2))
franke <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/franke2d.html
  test_func_apply(func=TF_franke, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_franke: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_franke(runif(2))
TF_franke <- function(x) {
  0.75 * exp(-(9 * x[1] - 2)^2 / 4 - (9 * x[2] - 2)^2 / 4) +
    0.75 * exp(-(9 * x[1] + 1)^2 / 49 - (9 * x[2] + 1)^2 / 10) +
    0.5 * exp(-(9 * x[1] - 7)^2 / 4 - (9 * x[2] - 3)^2 / 4) +
    -0.2 * exp(-(9 * x[1] - 4)^2 - (9 * x[2] - 7)^2)
}


#' zhou1998: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' zhou1998(runif(2))
zhou1998 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_zhou1998, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_zhou1998: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_zhou1998(runif(2))
TF_zhou1998 <- function(x) {
  # Any dim, http://www.sfu.ca/~ssurjano/zhou98.html
  d <- length(x)
  phi1 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 1 / 3))^2))
  phi2 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 2 / 3))^2))
  10^d / 2 * (phi1 + phi2)
}


#' currin1991: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' currin1991(runif(2))
currin1991 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/curretal91.html
  test_func_apply(func=TF_currin1991, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_currin1991: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_currin1991(runif(2))
TF_currin1991 <- function(x) {
  4.9 + 21.15 * x[1] - 2.17 * x[2] - 15.88 * x[1]^2 -
    1.38 * x[2]^2 - 5.26 * x[1] * x[2]
}


#' lim2002: Some function?
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' lim2002(runif(2))
lim2002 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/limetal02pol.html
  test_func_apply(func=TF_lim2002, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_lim2002: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_lim2002(runif(2))
TF_lim2002 <- function(x) {
  9 + 2.5 * x[1] - 17.5 * x[2] + 2.5 * x[1] * x[2] + 19 * x[2]^2 -
    7.5 * x[1]^3 - 2.5 * x[1] * x[2]^2 - 5.5 * x[2]^4 + x[1]^3 * x[2]^2
}


#' banana: A banana shaped function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' banana(runif(2))
#' x <- y <- seq(0, 1, len=100)
#' z <- outer(x, y, Vectorize(function(a, b){banana(c(a, b))}))
#' contour(x, y, z)
banana <- function(x, scale_it=T, scale_low = c(-20,-10), scale_high = c(20,5), noise=0) {
  # 2 dim, See Roshan SMED
  test_func_apply(func=TF_banana, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_banana: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_banana(runif(2))
TF_banana <- function(x){
  exp(-.005*x[1]^2-.5*(x[2]+.03*x[1]^2-3)^2)
}

#' gaussian1: A Gaussian function centered at 0.5.
#' Any dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' gaussian1(runif(2))
gaussian1 <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_gaussian1, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#gaussian1 <- standard_test_func(TF_gaussian1, scale_it=F, scale_low = c(0,0), scale_high = c(1,1))

#' TF_gaussian1: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param center Where to center the function, a vector.
#' @param s2 Variance of the Gaussian.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_gaussian1(runif(2))
TF_gaussian1 <- function(x, center=.5, s2=.01) {
  exp(-sum((x-center)^2/2/s2))
}

#' sinumoid: A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' sinumoid(runif(2))
#' x <- y <- seq(0, 1, len=100)
#' z <- outer(x, y, Vectorize(function(a, b){sinumoid(c(a, b))}))
#' contour(x, y, z)
sinumoid <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_sinumoid, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#sinumoid <- standard_test_func(TF_sinumoid, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0)
#' TF_sinumoid: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_sinumoid(runif(2))
TF_sinumoid <- function(x){
  sum(sin(2*pi*x*3)) + 20/(1+exp(-80*(x[[1]]-.5)))
}

#' waterfall: A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' waterfall(runif(2))
waterfall <- sinumoid


#' sqrtsin: A square root of a sine function.
#' Any dimensional function.
#' @param freq Wave frequency for sqrtsin and powsin
#' @export
#' @rdname test_func_apply
#' @examples
#' sqrtsin(runif(1))
#' curve(sqrtsin(matrix(x,ncol=1)))
sqrtsin <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0, freq=2*pi) {
  test_func_apply(func=TF_sqrtsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, freq=freq)
}
#' TF_sqrtsin: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param freq Wave frequency for TF_sqrtsin and TF_powsin
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_sqrtsin(runif(2))
TF_sqrtsin <- function(x, freq=2*pi) {
  ss <- sum(sin(freq*x))
  sqrt(abs(ss))*sign(ss)
}


#' powsin: A sine function raised to a power keeping its original sign.
#' Any dimensional function.
#' @rdname test_func_apply
#' @param pow Power for powsin
#' @export
#'
#' @examples
#' powsin(runif(1))#,pow=2)
powsin <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0, freq=2*pi, pow=.7) {
  test_func_apply(func=TF_powsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, freq=freq, pow=pow)
}
#powsin <- standard_test_func(TF_powsin, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), pow=1)

#' TF_powsin: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param pow Power to raise wave to for TF_powsin.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_powsin(runif(2))
TF_powsin <- function(x, freq=2*pi, pow=.7) {
  ss <- sum(sin(freq*x))
  (abs(ss) ^ pow) * sign(ss)
}

#' OTL_Circuit: OTL Circuit.
#' 6 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' OTL_Circuit(runif(6))
#' OTL_Circuit(matrix(runif(60),ncol=6))
OTL_Circuit <- function(x, scale_it=T, scale_low = c(50,25,0.5,1.2,0.25,50), scale_high = c(150,70,3,2.5,1.2,300), noise=0) {
  test_func_apply(func=TF_OTL_Circuit, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#OTL_Circuit <- standard_test_func(TF_OTL_Circuit, scale_it=T,
#                                  scale_low = c(50,25,0.5,1.2,0.25,50),
#                                  scale_high = c(150,70,3,2.5,1.2,300))

#' TF_OTL_Circuit: OTL Circuit function for evaluating a single point
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



#' GoldsteinPrice: Goldstein-Price function. Exponential scale, you
#' might want to use GoldsteinPriceLog instead
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' GoldsteinPrice(runif(2))
#' GoldsteinPrice(matrix(runif(60),ncol=2))
GoldsteinPrice <- function(x, scale_it=T, scale_low = c(-2,-2), scale_high = c(2,2), noise=0) {
  test_func_apply(func=TF_GoldsteinPrice, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_GoldsteinPrice: Goldstein Price function for evaluating a single point
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_GoldsteinPrice(c(0, -1)) # minimum
TF_GoldsteinPrice <- function(x) {
  a1 <- 1 + (x[1]+x[2]+1)^2 * (19-14*x[1]+3*x[1]^2-14*x[2]+6*x[1]*x[2]+3*x[2]^2)
  a2 <- 30 + (2*x[1]-3*x[2])^2 * (18-32*x[1]+12*x[1]^2+48*x[2]-36*x[1]*x[2]+27*x[2]^2)
  a1 * a2
}



#' GoldsteinPriceLog: Goldstein-Price function on a log scale.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' GoldsteinPriceLog(runif(2))
#' GoldsteinPriceLog(matrix(runif(60),ncol=2))
GoldsteinPriceLog <- function(x, scale_it=T, scale_low = c(-2,-2), scale_high = c(2,2), noise=0) {
  test_func_apply(func=TF_GoldsteinPriceLog, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_GoldsteinPrice: Goldstein Price function for evaluating a single point
#' on a log scale, normalized to have mean 0 and variance 1.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_GoldsteinPriceLog(c(0, -1)) # minimum
TF_GoldsteinPriceLog <- function(x) {
  a1 <- 1 + (x[1]+x[2]+1)^2 * (19-14*x[1]+3*x[1]^2-14*x[2]+6*x[1]*x[2]+3*x[2]^2)
  a2 <- 30 + (2*x[1]-3*x[2])^2 * (18-32*x[1]+12*x[1]^2+48*x[2]-36*x[1]*x[2]+27*x[2]^2)
  a3 <- a1 * a2
  (log(a3) - 8.693) / 2.427
}




#' ackley: Ackley function.
#' 2 dimensional function.
#' @param a A constant for ackley()
#' @param b A constant for ackley()
#' @param c A constant for ackley()
#' @export
#' @rdname test_func_apply
#' @examples
#' ackley(runif(2))
#' ackley(matrix(runif(60),ncol=2))
ackley <- function(x, scale_it=T, scale_low = -32.768, scale_high = 32.768, noise=0, a=20, b=0.2, c=2*pi) {
  test_func_apply(func=TF_ackley, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, a=a, b=b, c=c)
}

#' TF_ackley: Ackley function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @param a A constant for ackley()
#' @param b A constant for ackley()
#' @param c A constant for ackley()
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_ackley(c(0, 0)) # minimum of zero, hard to solve
TF_ackley <- function(x, a=20, b=0.2, c=2*pi) {
  -a * exp(-b*sqrt(mean(x^2))) - exp(mean(cos(c*x))) + a + exp(1)
}



#' piston: Piston simulation function.
#' 7 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' piston(runif(7))
#' piston(matrix(runif(7*20),ncol=7))
piston <- function(x, scale_it=T, scale_low = c(30,.005,.002,1e3,9e4,290,340), scale_high = c(60,.02,.01,5e3,11e4,296,360), noise=0) {
  test_func_apply(func=TF_piston, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_piston: Piston simulation function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_piston(c(30,.005,.002,1e3,9e4,290,340)) # minimum of zero, hard to solve
TF_piston <- function(x) {
  M <- x[1]
  S <- x[2]
  V0 <- x[3]
  k <- x[4]
  P0 <- x[5]
  Ta <- x[6]
  T0 <- x[7]
  A <- P0*S + 19.62*M -k*V0/S
  V <- S/(2*k) * (sqrt(A^2+4*k*P0*V0/T0*Ta) - A)
  C <- 2*pi * sqrt(M / (k + S^2*P0*V0/T0*Ta/V^2))
  C
}



#' wingweight: Wing weight function.
#' 10 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' wingweight(runif(10))
#' wingweight(matrix(runif(10*20),ncol=10))
wingweight <- function(x, scale_it=T, scale_low = c(150,220,6,-10,16,.5,.08,2.5,1700,.025), scale_high = c(200,300,10,10,45,1,.18,6,2500,.08), noise=0) {
  test_func_apply(func=TF_wingweight, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_wingweight: Wing weight function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_wingweight(c(150,220,6,-10,16,.5,.08,2.5,1700,.025)) # minimum of zero, hard to solve
TF_wingweight <- function(x) {
  Sw <- x[1]
  Wfw <- x[2]
  A <- x[3]
  Lambda <- x[4] * pi / 180 # convert degrees to radians
  q <- x[5]
  lambda <- x[6]
  tc <- x[7]
  Nz <- x[8]
  Wdg <- x[9]
  Wp <- x[10]
  0.036 * Sw^.758 * Wfw^.0035 * (A/cos(Lambda)^2)^.6 * q^.006 * lambda^.04 * (100*tc/cos(Lambda))^-.3 * (Nz*Wdg)^.49 + Sw*Wp
}




#' welch: Welch et al (1992) function.
#' 20 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' welch(runif(20))
#' welch(matrix(runif(20*20),ncol=20))
welch <- function(x, scale_it=T, scale_low = c(-.5), scale_high = c(.5), noise=0) {
  test_func_apply(func=TF_welch, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_welch: Welch function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_welch(rep(0,20)) # minimum of zero, hard to solve
TF_welch <- function(x) {
  5*x[12]/(1+x[1]) + 5*(x[4]-x[20])^2 + x[5] + 40*x[19]^3 +
    -5*x[19] + .05*x[2] + .08*x[3] - .03*x[6] +
    .03*x[7] - .09*x[9] - .01*x[10] - .07*x[11] +
    .25*x[13]^2 - .04*x[14] + .06*x[15] +
    -.01*x[17] - .03*x[18]
}






#' robotarm: Robot arm function.
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' robotarm(runif(8))
#' robotarm(matrix(runif(8*20),ncol=8))
robotarm <- function(x, scale_it=T, scale_low = rep(0,8), scale_high = c(rep(2*pi,4),rep(1,4)), noise=0) {
  test_func_apply(func=TF_robotarm, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_robotarm: Robot arm function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_robotarm(rep(0,8))
#' TF_robotarm(rep(1,8))
TF_robotarm <- function(x) {
  theta <- x[1:4]
  L <- x[5:8]
  u <- sum(L * cos(cumsum(theta)))
  v <- sum(L * sin(cumsum(theta)))
  sqrt(u^2 + v^2)
}





#' RoosArnold: Roos & Arnold (1963) function.
#' d dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' RoosArnold(runif(8))
#' RoosArnold(matrix(runif(8*20),ncol=8))
RoosArnold <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0) {
  test_func_apply(func=TF_RoosArnold, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_RoosArnold: Roos & Arnold (1963) function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_RoosArnold(rep(0,8))
#' TF_RoosArnold(rep(1,8))
TF_RoosArnold <- function(x) {
  prod(abs(4*x-2))
}





#' Gfunction: G-function
#' d dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' Gfunction(runif(8))
#' Gfunction(matrix(runif(8*20),ncol=8))
Gfunction <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_Gfunction, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_Gfunction: G-function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @param a Parameter for Gfunction
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_Gfunction(rep(0,8))
#' TF_Gfunction(rep(1,8))
TF_Gfunction <- function(x, a=(1:length(x)-1)/2) {
  prod((abs(4*x-2) + a) / (1 + a))
}






#' beale: Beale function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' beale(runif(2))
#' beale(matrix(runif(2*20),ncol=2))
beale <- function(x, scale_it=T, scale_low = -4.5, scale_high = 4.5, noise=0, ...) {
  test_func_apply(func=TF_beale, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_beale: Beale function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_beale(rep(0,2))
#' TF_beale(rep(1,2))
TF_beale <- function(x) {
  (1.5-x[1]+x[1]*x[2])^2 +
    (2.25-x[1]+x[1]*x[2]^2)^2 +
    (2.625 - x[1] + x[1]*x[2]^3)^2
}







#' easom: Easom function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' easom(runif(2))
#' easom(matrix(runif(2*20),ncol=2))
easom <- function(x, scale_it=T, scale_low = -4.5, scale_high = 4.5, noise=0, ...) {
  test_func_apply(func=TF_easom, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_easom: Easom function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_easom(rep(0,2))
#' TF_easom(rep(1,2))
TF_easom <- function(x) {
  -cos(x[1]) * cos(x[2]) * exp(-(x[1]-pi)^2 - (x[2]-pi)^2)
}








#' griewank: Griewank function
#' n dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' griewank(runif(2))
#' griewank(matrix(runif(2*20),ncol=2))
griewank <- function(x, scale_it=T, scale_low = -600, scale_high = 600, noise=0, ...) {
  test_func_apply(func=TF_griewank, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_griewank: Griewank function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_griewank(rep(0,2))
#' TF_griewank(rep(1,2))
TF_griewank <- function(x) {
  sum(x^2) / 400 - prod(cos(x/sqrt(1:length(x)))) + 1
}
