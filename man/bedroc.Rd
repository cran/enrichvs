\name{bedroc}
\alias{bedroc}

\title{
	The Boltzmann-Enhanced Discrimination of ROC (BEDROC)
}
\description{
	This program calculate BEDROC for virtual screening
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
	return a value, BEDROC
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
bedroc(dud_egfr[,1], dud_egfr[,2], decreasing=FALSE)
}
