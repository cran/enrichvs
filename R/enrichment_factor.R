# Calculate a enrichment factor for the highly-ranked compounds
# x: a vector for scores
# y: a vector for labels
#
# e.g.)
# > x <- rnorm(100001) - 1:100001 * 0.00005
# > y <- c(rep(1,501), rep(0,length(x)-501))
# > enrichment_factor(x, y, top=0.05)

enrichment_factor <- function(x, y, top=0.05, decreasing=TRUE) {
  if ( length(x) != length(y) ){
    stop(paste("The number of scores must be equal to the number of labels."))
  }
  N <- length(y)
  ord <- order(x, decreasing=decreasing)
  at <- round(N * top, 0)
  lig_sampled <- ( cumsum(y[ord]) / sum(y) )[at]
  n_sampled <- (1:length(y) / N )[at]

  return (lig_sampled / n_sampled) / ( sum(y) / N )
}
