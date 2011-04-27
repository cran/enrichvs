\name{auc}
\alias{auc}

\title{
	The Area Under the ROC Curve (AUC)
}
\description{
	This program calculate AUC for virtual screening
}
\usage{
	auc(x, y, decreasing=TRUE)
}

\arguments{
  \item{x}{
    a vector for scores
  }
  \item{y}{
    a vector for labels
  }
  \item{decreasing}{
    TRUE if the compounds are ranked by decreasing score
  }
}
\details{

}
\value{
	return a value, AUC
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
auc(x, y)

data(dud_egfr)
auc(dud_egfr[,1], dud_egfr[,2], decreasing=FALSE)
}
