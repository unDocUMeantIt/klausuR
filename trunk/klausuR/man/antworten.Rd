\encoding{UTF-8}
\name{antworten}
\Rdversion{1.1}
\alias{antworten}
\docType{data}
\title{
Sample dataset: antworten
}
\description{
Sample data of a multiple choice test.
}
\usage{data(antworten)}
\format{
  A data.frame with 20 observations of the following 37 variables.
  \describe{
    \item{\code{No}}{Serial number for each subject (numeric vector)}
    \item{\code{Name}}{Family name (character vector)}
    \item{\code{FirstName}}{First name (character vector)}
    \item{\code{MatrNo}}{Matriculation number (numeric vector)}
    \item{\code{Pseudonym}}{Pseudonym for anonymous feedback (character)}
    \item{\code{Item01} to \code{Item30}}{given answers, only one answer per item (numeric vector)}
    \item{\code{Item31} to \code{Item32}}{given answers, several answers per item (numeric vector)}
  }
}
\details{
This data was anonymised and Items 31 and 32 modified, to demonstrate items with multiple answers as well.
}
\source{
The data was taken from a final exam test in differential psychology at the Institute for Experimental
Psychology of the Heinrich-Heine-University Düsseldorf, summer term 2009.
}
\examples{
data(antworten)
}
\keyword{datasets}
