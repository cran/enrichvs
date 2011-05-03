\name{bedroc}
\alias{bedroc}

\title{
	Function to culculate the Boltzmann-Enhanced Discrimination of ROC (BEDROC)
}
\description{
	Function to culculate the Boltzmann-Enhanced Discrimination of ROC (BEDROC)
}
\usage{
	bedroc(x, y, decreasing=TRUE, alpha=20.0)
}

\arguments{
  \item{x}{
    a vector for scores
  }
  \item{y}{
    a vector for labels
  }
  \item{alpha}{
    coefficient alpha
  }
  \item{decreasing}{
    TRUE if the compounds are ranked by decreasing score
  }
}
\details{

}
\value{
	BEDROC, in the range from 0 to 1.
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
bedroc(x, y)

data(dud_egfr)
bedroc(dud_egfr$energy, dud_egfr$label, decreasing=FALSE)
}
