
dago_wrapper <- function(x){
  A <- Coereba:::dagoTest(x)
  method <- A@test$method
  p.value <- A@test$p.value[[1]]
  C <- data.frame(cbind(p.value, method))
}