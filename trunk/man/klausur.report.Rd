\name{klausur.report}
\alias{klausur.report}
\title{Generate individual reports on multipe choice test results}
\usage{klausur.report(klsr, matn, save=FALSE, pdf=FALSE, path=NULL,
file.name="matn", hist=list(points=FALSE, marks=FALSE),
hist.points="hist_points.pdf", hist.marks="hist_marks.pdf",
marks.info=list(points=FALSE, percent=FALSE),
descr=list(title=NULL, name=NULL, date=NULL),
lang="en", alt.candy=TRUE, anon.glob.file="anon.tex")}
\description{\code{klausur.report} takes (at least) an object of class klausuR (or klausuR.mult) and a matriculation number to generate personal test results
in LaTeX and/or PDF format.}
\details{The report contains, next to the individual results, a table with all given and correct  answers (using \code{\link[xtable]{xtable}}),
as well as nice histograms showing the distribution of the test results (points and/or marks are supportet). If the matriculation numer
is set to "all", reports for all subjects are produced. Setting it to "anon" will get you a printable version of the anonymized results.

By default output is sent to standard out. To save them to disk in LaTeX format a "save" parameter is provided. Alternatively, the reports
can be converted to PDF format as well. \code{klausur.report} is calling \code{\link[texi2dvi]{texi2dvi}} from the \code{tools} package for that.

If the object is of class klausuR.mult, only the global results for tests with several test forms are evaluated. In case you'd rather like
reports on each test form, call \code{klausur.report} with the single slots from that object accordingly.}
\alias{klausur.report}
\keyword{IO}
\keyword{file}
\value{One or several LaTeX documents. If defined two histograms will be plotted.}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\seealso{\code{\link[klausuR:klausur]{klausur}}, \code{\link[xtable]{xtable}}, \code{\link[texi2dvi]{texi2dvi}}}
\arguments{\item{klsr}{An object of class klausuR or klausuR.mult.}
\item{matn}{Matriculation number, "all" (produces individuall documents for all subjects), "anon" (produces anonymous feedback)
or "glob" (produces a global results document).}
\item{save}{logical: If TRUE, files are saved to disk (scheme: "\code{path}/\code{matn}.tex").}
\item{pdf}{logical: If TRUE, LaTeX reports will be converted to PDF automatically, using \code{\link[texi2dvi]{texi2dvi}}.
If \code{save} is FALSE, a temporary directory is used, that is only the PDF files will be saved.}
\item{path}{Path for \code{save} and \code{hist} files.}
\item{file.name}{File name scheme for the reports, either "matn" (matriculation number) or "name" (name and firstname).}
\item{hist}{A list with the logical elements \code{points} and \code{marks}: If TRUE, the reports will include histograms
of the distribution of points and/or marks. The needed PDF files will be created by \code{\link[plot,klausuR]{plot}} and saved as well.
(see \code{path}, \code{hist.points} and \code{hist.marks}).}
\item{hist.points}{File name for the histogram of points.}
\item{hist.marks}{File name for the histogram of marks.}
\item{descr}{Details on the test: List with the elements \code{title} (title of the test), \code{name} (your name) and \code{date}.}
\item{marks.info}{A list with the logical elements \code{points} and \code{percent}: If TRUE, the reports will include a table showing
how marks were assigned to points achieved and/or percent solved, respectively.}
\item{lang}{Set to "de" for reports in German, English is the default.}
\item{alt.candy}{If TRUE, a comma will be inserted for items with multiple alternatives ("235" becomes "2, 3, 5" in the printout)}
\item{anon.glob.file}{If \code{matn="anon"} or \code{matn="glob"}, you can specify a filename for this particular report.}
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
klausur.report(klsr=klsr.obj, matn="all", descr=list(title="Klausur Tatort",
name="Dr. T. Aeter", date="24.09.2010"))}

