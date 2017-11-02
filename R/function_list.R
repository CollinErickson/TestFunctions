
# Name, dimensions, scale low, scale high, parameters, references
TestFunctions_list <- list(
  c("branin", "2", "c(-5, 0)", "c(10, 15)", "a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)", "Dixon, L. C. W. (1978). The global optimization problem: an introduction. Towards Global Optimiation 2, 1-15."),
  c("borehole", "8", "c(.05,100,63070,990,63.1,700,1120,9855)","c(.15,50000,115600,1110,116,820,1680,12045)","", "Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.")
)

TestFunctions_df <- plyr::ldply(TestFunctions_list, identity)

names(TestFunctions_df) <- c("Function", "Dim", "Scale low", "Scale high", "Parameters", "Reference")
