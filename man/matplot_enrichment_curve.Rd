\name{matplot_enrichment_curve}
\alias{matplot_enrichment_curve}

\title{
	Plot multiple enrichment curves for virtual screening
}
\description{
	Plot multiple enrichment curves for virtual screening
}
\usage{
	matplot_enrichment_curve(x, y)
}

\arguments{
  \item{x}{
    score matrix for compounds (row) by several approarches (column)
  }
  \item{y}{
    activity label for compounds (0:inactive, 1:active)
  }
}
\details{

}
\value{
	call a plot
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
x <- matrix(rnorm(5000), ncol=5)  # random 5 scores for 1000 compounds
y <- c(rep(1,50), rep(0,950))     # activity labels for "x"
matplot_enrichment_curve(x, y)
}
