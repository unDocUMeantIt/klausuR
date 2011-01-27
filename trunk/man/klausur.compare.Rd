\name{klausur.compare}
\alias{klausur.compare}
\title{Comparison of data sets}
\usage{klausur.compare(set1, set2, select=NA, new.set=NA)}
\description{Compare two data sets for use with klausuR}
\details{The function \code{klausur.compare} will take two data sets and compare them for equality. This is useful to check for typos before
you calculate the results with \code{\link[klausuR:klausur]{klausur}}. If you need to type in the given answers by hand, errors 
easily occur, so it is advisable to input all data at least twice (perhaps by different persons) and check for differences
with this function, which can then be corrected by looking up the original answer in the test.

If you don't want to compare all variables but only a subset, you can use the \code{select} option (see examples below).
But be careful with this, at least the variables "No", "FirstName" and "Name" are needed for the output!

For convenience, if \code{new.set} is set to a character string, a new object is created with this name. It contains the data
that is identical in both sets compared, but all dubious values will be replaced by NA.}
\alias{klausur.compare}
\value{A data.frame of the differences, if found. If not, just a message is returned.}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\keyword{utilities}
\seealso{\code{\link[klausuR:klausur]{klausur}}}
\arguments{\item{set1,set2}{The data sets to be compared}
\item{select}{A vector with variables that should be compared, all others are omitted. At least "No", "FirstName" and "Name" are needed for the output!}
\item{new.set}{A character string representing a valid name for an R object}
}
\examples{\dontrun{
# default comparison
klausur.compare(antworten, antworten2)

# compare only variables 1 to 12
klausur.compare(antworten, antworten2, select=c(1:12))

# omit variables 3 to 8 and create a new set called "antworten.comp"
# from the results
klausur.compare(antworten, antworten2, select=-c(3:8),
new.set="antworten.comp")
}}

