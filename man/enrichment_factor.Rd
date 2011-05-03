\name{enrichment_factor}
\alias{enrichment_factor}

\title{
	Function to calculate the enrichment factor (EF)
}
\description{
	Function to calculate the enrichment factor (EF)
}
\usage{
	enrichment_factor(x, y, top=0.05, decreasing=TRUE)
}

\arguments{
  \item{x}{
    a vector for scores
  }
  \item{y}{
    a vector for labels
  }
  \item{top}{
    thresholed ratio of top-ranked compound
  }
  \item{decreasing}{
    TRUE if the compounds are ranked by decreasing score
  }
}
\details{

}
\value{
	EF, in the range from 0 to +Inf.
}
\references{

}
\author{
	Hiroaki YABUUCHI
}
\note{

}

\seealso{

}
\examples{
x <- rnorm(1000)  # random scores for 1000 compounds
y <- c(rep(1,50), rep(0,950))     # activity labels for "x"
enrichment_factor(x, y, top=0.1)

data(dud_egfr)
enrichment_factor(dud_egfr$energy, dud_egfr$label, decreasing=FALSE)

}
