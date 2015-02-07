\encoding{UTF-8}
\name{antworten.mufo}
\Rdversion{1.1}
\alias{antworten.mufo}
\alias{corr.key}
\docType{data}
\title{
Sample dataset: antworten.mufo
}
\description{
Sample data of a multiple choice test with two test forms.
}
\usage{data(antworten.mufo)}
\format{
  A data.frame with 20 observations of the following 38 variables.
  \describe{
    \item{\code{No}}{Serial number for each subject (numeric vector)}
    \item{\code{Name}}{Family name (character vector)}
    \item{\code{FirstName}}{First name (character vector)}
    \item{\code{MatrNo}}{Matriculation number (numeric vector)}
    \item{\code{Pseudonym}}{Pseudonym for anonymous feedback (character)}
    \item{\code{Item01} to \code{Item30}}{given answers,
      only one answer per item (numeric vector)}
    \item{\code{Item31} to \code{Item32}}{given answers,
      several answers per item (numeric vector)}
    \item{\code{Form}}{Test form (character vector)}
  }
  And a matrix called \code{corr.key} with indices for all items in the two test forms.
}
\details{
The data is actually the same as \code{antworten}. But in this object the items of the last 10 test
subjects were reversed, hence this is test form ''b''. As a convenience,
      a matrix called \code{corr.key}
is also included. This data can be used to test \code{klausur.mufo}.
}
\source{
The data was taken from a final exam test in differential psychology at the Institute for Experimental
Psychology of the Heinrich-Heine-University Düsseldorf, summer term 2009.
}
\examples{
data(antworten.mufo)
}
\keyword{datasets}
