\name{klausur.gen}
\alias{klausur.gen}
\title{Generate data sets for the use with klausur}
\usage{klausur.gen(items, obs=1, items.char=FALSE)
}
\description{Generate empty data sets for the use with klausur}
\value{A data.frame containing the variables "No", "Name", "FirstName", "MatrNo", "Pseudonym" and the number of items as needed.}
\seealso{\code{\link[klausuR:klausur]{klausur}}, \code{\link[klausuR:klausur.compare]{klausur.compare}},
\code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}, \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}}}
\keyword{datagen}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\arguments{\item{items}{an integer declaring the number of items to be created}
\item{obs}{integer, numer ob observations}
\item{items.char}{logical, will the answers be coded as characters or integer numbers (default)?}
}
\examples{antworten2 <- klausur.gen(items=20,obs=40)}

