
# Name, dimensions, scale low, scale high, parameters, references
TestFunctions_list <- list(
  c("branin", "2", "2", "2", "c(-5, 0)", "c(10, 15)", "a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)", "Dixon, L. C. W. (1978). The global optimization problem: an introduction. Towards Global Optimiation 2, 1-15."),
  c("borehole", "8", "8", "8", "c(.05,100,63070,990,63.1,700,1120,9855)","c(.15,50000,115600,1110,116,820,1680,12045)","", "Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.")
)

TestFunctions_df <- plyr::ldply(TestFunctions_list, identity)

names(TestFunctions_df) <- c("Function", "Dim", "Scale low", "Scale high", "Parameters", "Reference")

#' Details of all test functions
#'
#' A data frame containing the details of the functions provided
#' in the TestFunctions package.
#' The variables are as follows:
#'
#' \itemize{
#'   \item price. price in US dollars (\$326--\$18,823)
#'   \item carat. weight of the diamond (0.2--5.01)
#'   \item cut. quality of the cut (Fair, Good, Very Good, Premium, Ideal)
#'   \item colour. diamond colour, from J (worst) to D (best)
#'   \item clarity. a measurement of how clear the diamond is (I1 (worst), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (best))
#'   \item x. length in mm (0--10.74)
#'   \item y. width in mm (0--58.9)
#'   \item z. depth in mm (0--31.8)
#'   \item depth. total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
#'   \item table. width of top of diamond relative to widest point (43--95)
#' }
#'
#' @importFrom plyr ldply
#' @docType data
#' @keywords datasets
# @name diamonds
#' @usage data(TestFunctions_df)
#' @format A data frame with 53940 rows and 10 variables
"TestFunctions_df"


# Trying second format
N <- 10
TestFunctions_list2 <- data.frame(name=character(N),
                                  D=character(N),
                                  D_min=numeric(N),
                                  D_max=numeric(N),
                                  scale_low=numeric(N), # Use I(c(values))
                                  scale_high=numeric(N),
                                  parameters=I(vector("list", N)),
                                  reference=character(N),
                                  stringsAsFactors = FALSE
)


TestFunctions_list2[1,] <- list("branin", "2",2, 2, I(list(c(-5, 0))), I(list(c(10, 15))), I(list(c(a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)))), "Dixon, L. C. W. (1978). The global optimization problem: an introduction. Towards Global Optimiation 2, 1-15.")
TestFunctions_list2[2,] <- list("borehole", "8", 8, 8, I(list(c(.05,100,63070,990,63.1,700,1120,9855))),I(list(c(.15,50000,115600,1110,116,820,1680,12045))),NA, "Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.")
TestFunctions_list2[3,] <- list("franke", "2", 2, 2, I(list(c(0,0))), I(list(c(1,1))), NA, "Franke, R. (1979). A critical comparison of some methods for interpolation of scattered data. Monterey, California: Naval Postgraduate School.")
TestFunctions_list2[4,] <- list("zhou98", "1-Inf", 1, Inf, I(list(0)), I(list(1)), NA, "An, J., & Owen, A. (2001). Quasi-regression. Journal of complexity, 17(4), 588-607.")
TestFunctions_list2[5,] <- list("currin1991", "2", 2, 2, I(list(c(0,0))), I(list(c(0,0))), NA, "Currin, C., Mitchell, T., Morris, M., & Ylvisaker, D. (1991). Bayesian prediction of deterministic functions, with applications to the design and analysis of computer experiments. Journal of the American Statistical Association, 86(416), 953-963.")


# Template
# TestFunctions_list2[i,] <- list("", "", , , I(list()), I(list()), I(list()), "")
