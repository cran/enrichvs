\name{dud_egfr}
\alias{dud_egfr}
\docType{data}
\title{
	Scores and labels for EGFR ligands/docoys from DUD dataset
}
\description{
	Ligands and decoys for Epidermal Growth Factor Receptor (EGFR) were scored by DOCK programs (Huang et al., 2006).
}
\usage{data(dud_egfr)}
\format{
  A list with 88888 observations on the following 2 variables.
  \describe{
    \item{\code{energy1}}{ -99.7 -98.3 -97.4 ... }
    \item{\code{label}}{ 0 0 0 ... }
  }
}

\details{
  energy : a numeric vector for scores (binding free energies)
  label : a numeric vector for labels (1:ligand, 0:decoy)
}
\source{
	A Directory of Useful Decoys (DUD): http://dud.docking.org/r2/
}
\references{
	Huang N. et al. Benchmarking Sets for Molecular Docking. J. Med. Chem. 49, 6789-6801, (2006).
}
\examples{
	data(dud_egfr)
	enrichment_factor(dud_egfr$energy, dud_egfr$label)
}
\keyword{datasets}
