\docType{methods}
\name{sort.klausuR}
\alias{sort,-methods}
\alias{sort,klausuR-method}
\alias{sort.klausuR}
\alias{sort.klausuR,klausuR-method}
\title{Sort method for S4 objects of class klausuR}
\usage{
sort.klausuR(x, decreasing = FALSE, ...)

\S4method{sort.klausuR}{klausuR}(x, decreasing = FALSE, sort.by = c())
}
\arguments{
  \item{x}{An object of class \code{klausuR}}

  \item{decreasing}{Logical, whether sorting should be sone
  increasing or decreasing.}

  \item{...}{Additional arguments.}

  \item{sort.by}{An optional character string naming a
  variable to sort the results by. Defaults to \code{c()},
  i.e. no re-ordering.}
}
\description{
Returns the given object, with global results and
anonymized results sorted by the given variable.
}
\examples{
\dontrun{
klsr.obj <- klausur(data.obj)
sort(klsr.obj, sort.by="Points")
}
}
\author{
m.eik michalke \email{meik.michalke@uni-duesseldorf.de}
}
\seealso{
\code{\link[klausuR:klausur]{klausur}}
}
\keyword{methods}

