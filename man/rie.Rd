\name{rie}
\alias{rie}

\title{
	Robust Initial Enhancement (RIE)
}
\description{
	This program calculate RIE for virtual screening
}
\usage{
	rie(x, y, decreasing=TRUE, alpha=20.0)
}

\arguments{
  \item{x}{
    a vector for scores
  }
  \item{y}{
    a vector for labels (0:inactive, 1:active)
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
	return a value, RIE
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
rie(x, y)

data(dud_egfr)
rie(dud_egfr[,1], dud_egfr[,2], decreasing=FALSE)

}
