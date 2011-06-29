\name{auc}
\alias{auc}

\title{
	Function to calculate the Area Under the ROC Curve (AUC)
}
\description{
	Function to calculate the Area Under the ROC Curve (AUC)
}
\usage{
	auc(x, y, decreasing=TRUE, top=1.0)
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
  \item{top}{
    threshold ratio of the false positives (when ROC analysis is performed on a top list)
  }
}
\details{

}
\value{
	AUC, in the range from 0 to 1.
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
auc(dud_egfr$energy, dud_egfr$label, decreasing=FALSE)

# AUC up to the first 10% of the false positives (ROC10%)
auc(dud_egfr$energy, dud_egfr$label, decreasing=FALSE, top=0.1)

}
