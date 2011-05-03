# Calculate the Area Under the Accumulation Curve (AUAC)
# x: a vector for scores
# y: a vector for labels
#
# e.g.)
# > x <- rnorm(100001) - 1:100001 * 0.00005
# > y <- c(rep(1,501), rep(0,length(x)-501))
# > auac(x, y, decreasing=TRUE)
#
# Ref.: Truchon et al. Evaluating Virtual Screening Methods: 
#   Good and Bad Metrics for the "Early Recognition" Problem.
# J. Chem. Inf. Model. (2007) 47, 488-508.
#

auac <- function(x, y, decreasing=TRUE) {
  if ( length(x) != length(y) ){
    stop(paste("The number of scores must be equal to the number of labels."))
  }
  N <- length(y)
  n <- length( which(y==1) )
  ord <- order(x, decreasing=decreasing)
  m_rank <- which( y[ord] == 1 )
  return( 1 - sum( m_rank ) / n / N )
}
