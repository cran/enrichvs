\name{rie}
\alias{rie}

\title{
	Function to culculate the Robust Initial Enhancement (RIE)
}
\description{
	Function to culculate the Robust Initial Enhancement (RIE)
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
	RIE, in the range from 0 to +Inf.
}
\references{
	Sheridan RP et al. Protocols for bridging the peptide to nonpeptide gap in topological similarity searches. J. Chem. Inf. Comput. Sci. (2001) 41, 1395-1406.

	Truchon et al. Evaluating Virtual Screening Methods: Good and Bad Metrics for the "Early Recognition" Problem. J. Chem. Inf. Model. (2007) 47, 488-508.

}
\author{
	Hiroaki YABUUCHI
}
\note{
	Though RIE was originally calculated through a Monte Carlo simulation, 
	this function adopts a simplified formulation proposed by Truchon et al.
	to reduce computational cost.
}

\seealso{

}
\examples{
x <- rnorm(1000)  # random scores for 1000 compounds
y <- c(rep(1,50), rep(0,950))     # activity labels for "x"
rie(x, y)

data(dud_egfr)
rie(dud_egfr$energy, dud_egfr$label, decreasing=FALSE)

}
