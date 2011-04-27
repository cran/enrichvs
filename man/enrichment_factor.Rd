\name{enrichment_factor}
\alias{enrichment_factor}

\title{
	Enrichment factor calculation
}
\description{
	This program calculate enrichment factors of virtual screening
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
	a value, EF
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
enrichment_factor(dud_egfr[,1], dud_egfr[,2], decreasing=FALSE)

}
