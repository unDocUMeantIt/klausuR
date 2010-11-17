\name{show-methods}
\alias{show-methods}
\alias{show}
\title{Show methods for objects of class klausuR and klausuR.mult}
\description{Show methods for S4 objects of classes klausuR and klausuR.mult}
\details{\code{show-methods}: Print a nice summary of the slots \code{results}, \code{anon}, \code{cronbach} and \code{item.analysis}.
If object is of class klausuR.mult, the global results for tests with several test forms are evaluated.

}
\alias{show,-methods}
\alias{show,klausuR-method}
\alias{show,klausuR.mult-method}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\seealso{\code{\link[klausuR:klausur]{klausur}}, \code{\link[klausuR:klausur.mufo]{klausur.mufo}}}
\keyword{methods}
\arguments{\item{object}{An object of class \code{klausuR} or \code{klausuR.mult}}
}
\examples{\dontrun{
klausur(answ=antworten, corr=richtig, marks=notenschluessel)

# multiple test forms
klausur.mufo(answ=antworten, corr=richtig, corr.key=testformen,
marks=notenschluessel)
}}
\alias{show,-method}
\alias{show}
\alias{show,-method}
\alias{show}

