# Calculate a BEDROC for the highly-ranked compounds
# x: a vector for scores
# y: a vector for labels
#
# e.g.)
# > x <- rnorm(100001) - 1:100001 * 0.00005
# > y <- c(rep(1,501), rep(0,length(x)-501))
# > bedroc(x, y, alpha=20.0, decreasing=TRUE)

bedroc <- function(x, y, decreasing=TRUE, alpha=20.0) {
  N <- length(y)
  n <- length( which(y==1) )
  ord <- order(x, decreasing=decreasing)
  m_rank <- which( y[ord] == 1 )
  s <- sum( exp(-alpha * m_rank / N ) )
  ra <- n / N
  ri <- (N - n) / N
  random_sum <- ra * exp( -alpha / N )*(1.0 - exp( -alpha ) )/ ( 1.0 - exp( -alpha / N ) )
  fac <- ra * sinh( alpha / 2.0 ) / ( cosh(alpha / 2.0 ) - cosh(alpha / 2.0 - alpha * ra ) )
  cte = 1.0/ ( 1 - exp( alpha * ri) )
  return( s / random_sum * fac + cte )
}
