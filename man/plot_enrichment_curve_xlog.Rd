\name{plot_enrichment_curve_xlog}
\alias{plot_enrichment_curve_xlog}

\title{
	Function to plot a enrichment curve with a logarithmic scale on the x axis
}
\description{
	Function to plot a enrichment curve with a logarithmic scale on the x axis
}
\usage{
	plot_enrichment_curve_xlog(x, y, decreasing=TRUE, nplot=100, 
		colbarwidth=1, colorize=FALSE, add=FALSE, col="black")
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
  \item{nplot}{
    the number of plots
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
x <- rnorm(1000)    # random scores for 1000 compounds
y <- c(rep(1,50), rep(0,950))     # activity labels for "x"
plot_enrichment_curve_xlog(x, y)

data(dud_egfr)
plot_enrichment_curve_xlog(dud_egfr$energy, dud_egfr$label, 
	add=TRUE, decreasing=FALSE, col="red")
}
