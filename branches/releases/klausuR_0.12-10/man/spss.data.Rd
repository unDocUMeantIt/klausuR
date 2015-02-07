\encoding{UTF-8}
\name{spss.data}
\Rdversion{1.1}
\alias{spss.data}
\docType{data}
\title{
Sample dataset: spss.data
}
\description{
Sample data of a test with NRET data.
}
\usage{data(spss.data)}
\format{
  A data.frame with 17 observations of the following 35 variables.
  \describe{
    \item{\code{No}}{Serial number for each subject (numeric vector)}
    \item{\code{MatrNo}}{Matriculation number (character vector)}
    \item{\code{FirstName}}{First name (character vector)}
    \item{\code{Name}}{Family name (character vector)}
    \item{\code{Nickname}}{Pseudonym for anonymous feedback,
      needs to be renamed (character vector)}
    \item{\code{item01a1} to \code{item10a3}}{Given answers, three answers for each item,
      i.e. one for each alternative (numeric vector)}
  }
}
\details{
This data was anonymised and combined from originally 51 observations. See \code{\link[klausuR:nret.translator]{nret.translator}} on details
how the answers are coded.
}
\source{
The data was taken from a final exam test in diagnostic methods at the Institute for Experimental
Psychology of the Heinrich-Heine-University Düsseldorf, summer term 2011.
}
\examples{
data(spss.data)
}
\keyword{datasets}
