\name{klausur.gen.corr}
\alias{klausur.gen.corr}
\title{A function to generate a vector with correct answers}
\usage{
  klausur.gen.corr(answ=NULL, items.char=FALSE,
  test.forms=1)
}
\arguments{
  \item{answ}{Either an object with item names in klausuR
  scheme (see \code{\link[klausuR:klausur]{klausur}}), e.g.
  your observation data, or an integer representing the
  maximum score of the test. If NULL, you will be asked for
  the maximum score.}

  \item{items.char}{Logical, will the answers be coded as
  characters or integer numbers (default)?}

  \item{test.forms}{An integer value specifying how many
  parallel test forms are available}
}
\value{
  A numeric or character vector (depending on the parameter
  \code{items.char}).
}
\description{
  Create a vector of correct answers to be used by
  \code{\link[klausuR:klausur]{klausur}}.
}
\details{
  By default answers are expected to be numeric values. You
  can change that to character with \code{items.char=TRUE}.

  The parameter \code{answ} is quite versatile. You can
  just feed it your observation data, if it complies with
  the naming scheme for items (\code{Item###}, see also
  \code{\link[klausuR:klausur.gen]{klausur.gen}}), and
  \code{klausur.gen.corr} will use all of its items
  automatically. Or you assign the number of items directly
  as an integer value. If you leave \code{answ=NULL}, you
  will be asked for the number of items.
}
\examples{
\dontrun{
richtig <- klausur.gen.corr(answ=antworten)
}
}
\author{
  m.eik michalke \email{meik.michalke@uni-duesseldorf.de}
}
\seealso{
  \code{\link[klausuR:klausur]{klausur}}
}
\keyword{utilities}

