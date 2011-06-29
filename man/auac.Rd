\name{auac}
\alias{auac}

\title{
	Function to culculate the Area Under the Accumulation Curve (AUAC)
}
\description{
	Function to calculate the Area Under the Accumulation Curve (AUAC).
}
\usage{
	auac(x, y, decreasing=TRUE, top=1.0)
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
  \item{top}{
    threshold ratio of the false positives (when ROC analysis is performed on a top list)
  }
}
\details{

}
\value{
	AUAC, in the range from 0 to 1.
}
\references{
	Truchon et al. Evaluating Virtual Screening Methods: Good and Bad Metrics for the "Early Recognition" Problem. J. Chem. Inf. Model. (2007) 47, 488-508.
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
auac(x, y)

data(dud_egfr)
auac(dud_egfr$energy, dud_egfr$label, decreasing=FALSE)
}
