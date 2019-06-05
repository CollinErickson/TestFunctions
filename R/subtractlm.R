subtractlm <- function(func, d, n=d*100) {
  x1 <- lhs::randomLHS(n, d)
  y <- apply(x1, 1, func)
  df <- data.frame(x=x1, y=y)
  mod_lm <- lm(y ~ ., df)

  function(x, ...) {
    func(x, ...) - predict(mod_lm, data.frame(x=if (is.matrix(x)) {x} else {t(x)}))
  }
}

if (F) {
  curve(subtractlm(. %>% .^2, 1)(x))
  funcprofile(subtractlm(. %>% .^2, 1), 1)
}
