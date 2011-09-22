\name{show}
\alias{show}
\alias{show,klausuR-method}
\alias{show,klausuR.mult-method}
\alias{show,-methods}
\title{Show methods for objects of class klausuR and klausuR.mult}
\arguments{
  \item{object}{An object of class \code{klausuR} or
  \code{klausuR.mult}}
}
\description{
  Show methods for S4 objects of classes klausuR and
  klausuR.mult
}
\details{
  Print a nice summary of the slots \code{results},
  \code{anon}, \code{cronbach} and \code{item.analysis}. If
  object is of class klausuR.mult, the global results for
  tests with several test forms are evaluated.
}
\examples{
\dontrun{
klausur(data.obj)

# multiple test forms
klausur.mufo(data.obj, marks=notenschluessel)
}
}
\author{
  m.eik michalke \email{meik.michalke@uni-duesseldorf.de}
}
\seealso{
  \code{\link[klausuR:klausur]{klausur}},
  \code{\link[klausuR:klausur.mufo]{klausur.mufo}}
}
\keyword{methods}

