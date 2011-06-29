\name{plot_enrichment_curve}
\alias{plot_enrichment_curve}

\title{
	Function to plot a enrichment curve for virtual screening
}
\description{
	Function to plot a enrichment curve for virtual screening
}
\usage{
	plot_enrichment_curve(x, y, decreasing=TRUE, npoint=100, 
		colbarwidth=1, colorize=FALSE, add=FALSE, col="black", lwd=2)
}

\arguments{
  \item{x}{
    a vector for scores
  }
  \item{y}{
   a vector for labels (0:inactive, 1:active)
  }
  \item{decreasing}{
    TRUE if the compounds are ranked by decreasing score
  }
  \item{npoint}{
    the number of points to plot the curve
  }
  \item{colbarwidth}{
    the width of color bar (only if colorize==TRUE)
  }
  \item{colorize}{
    TRUE if the plot line is colorerd by the corresponding scores
  }
  \item{add}{
    TRUE if the plot is add to the previous plot
  }
  \item{col}{
    color of the plot
  }
  \item{lwd}{
    line width of the plot
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
data(dud_egfr)
x <- rnorm( 88888 )                        # virtual scores for 1000 compounds
y <- c(rep(1,88), rep(0,88800))            # activity labels for "x"
plot_enrichment_curve(x, y, col="blue")

plot_enrichment_curve(dud_egfr$energy, dud_egfr$label, 
	add=TRUE, decreasing=FALSE, col="red") # Add a plot
legend("bottomrigh", legend = c("ideal", "random", "score1", "score2"),
	lty=c(2,3,1,1), col=c("black", "grey", "blue", "red"), bty="n")

}
