# Calculate a enrichment factor for the highly-ranked compounds
# x: a vector for scores
# y: a vector for labels
#
# e.g.)
# > x <- rnorm(100001) - 1:100001 * 0.00005
# > y <- c(rep(1,501), rep(0,length(x)-501))
# > enrichment_factor(x, y, top=0.05)

enrichment_factor <- function(x, y, top=0.05, decreasing=TRUE) {
  ord <- order(x, decreasing=decreasing)
  at <- round(length(y) * top, 0)
  lig_sampled <- ( cumsum(y[ord]) / sum(y) )[at]
  n_sampled <- (1:length(y) / length(y) )[at]

  return (lig_sampled / n_sampled) / ( sum(y) / length(y) )
}
