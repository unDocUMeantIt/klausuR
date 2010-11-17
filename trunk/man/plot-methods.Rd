\name{plot-methods}
\alias{plot-methods}
\alias{plot}
\title{plot methods for klausuR and klausuR.mult}
\usage{plot(x, marks=FALSE, sd.lines=FALSE, plot.normal=TRUE, \dots)}
\description{Plot methods for S4 objects of class klausuR and klausuR.mult}
\details{\code{plot-methods}: These plot methods are beeing called by \code{\link[klausuR:klausur.report]{klausur.report}}.
If \code{x} is of class \code{klausuR.mult}, only the global results will be plotted.
Should you rather like plots on each test form, call \code{plot} with the single slots from that object accordingly.

}
\alias{plot,klausuR-method}
\alias{plot,klausuR.mult-method}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\seealso{\code{\link[klausuR:klausur]{klausur}}, \code{\link[klausuR:klausur.mufo]{klausur.mufo}}, \code{\link[klausuR:klausur.report]{klausur.report}}}
\keyword{methods}
\keyword{plot}
\arguments{\item{x}{An S4 object of class \code{klausuR} or \code{klausuR.mult}}
\item{marks}{Logical, whether the histogram should show the distribution of points (default) or marks}
\item{sd.lines}{Logical, whether standard deviation lines should be plotted}
\item{plot.normal}{Logical, whether normal distribution should be plotted (according to mean and Sd of the results)}
\item{\dots}{Any other argument suitable for plot()}
}
\examples{data(antworten)

# vector with correct answers:
richtig <- c(Item01=3, Item02=2, Item03=2, Item04=2, Item05=4,
Item06=3, Item07=4, Item08=1, Item09=2, Item10=2, Item11=4,
Item12=4, Item13=2, Item14=3, Item15=2, Item16=3, Item17=4,
Item18=4, Item19=3, Item20=5, Item21=3, Item22=3, Item23=1,
Item24=3, Item25=1, Item26=3, Item27=5, Item28=3, Item29=4,
Item30=4, Item31=13, Item32=234)

# vector with assignement of marks:
notenschluessel <- c()
# scheme of assignments: marks[points_from:to] <- mark
notenschluessel[0:12]  <- 5.0
notenschluessel[13:15] <- 4.0
notenschluessel[16:18] <- 3.7
notenschluessel[19:20] <- 3.3
notenschluessel[21]    <- 3.0
notenschluessel[22]    <- 2.7
notenschluessel[23]    <- 2.3
notenschluessel[24]    <- 2.0
notenschluessel[25:26] <- 1.7
notenschluessel[27:29] <- 1.3
notenschluessel[30:32] <- 1.0

klsr.obj <- klausur(answ=antworten, corr=richtig, marks=notenschluessel)
plot(klsr.obj, marks=TRUE)}
\alias{plot,-method}
\alias{plot}
\alias{plot,-method}
\alias{plot}

