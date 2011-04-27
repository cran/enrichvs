# Calculate a RIE for the highly-ranked compounds
# x: a vector for scores
# y: a vector for labels
#
# e.g.)
# > x <- rnorm(100001) - 1:100001 * 0.00005
# > y <- c(rep(1,501), rep(0,length(x)-501))
# > rie(x, y, alpha=20.0, decreasing=TRUE)

rie <- function(x, y, decreasing=TRUE, alpha=20.0) {
  N <- length(y)
  n <- length( which(y==1) )
  ord <- order(x, decreasing=decreasing)
  m_rank <- which( y[ord] == 1 )
  s <- sum( exp(-alpha * m_rank / N ) )
  ra <- n / N
  ri <- (N - n) / N
  random_sum <- (n / N ) * (1 - exp(-alpha)) / ( exp(alpha / N) - 1 )
  return( s / random_sum )
}
