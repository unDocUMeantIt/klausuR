\name{grand.table}
\alias{grand.table}
\title{Export results to a table}
\usage{grand.table(NR.res=NULL, NRET.res=NULL, NRETp.res=NULL, ET.res=NULL,
rescale=TRUE, file=NULL, csv2=TRUE, encoding="CP1252", ...)}
\description{Try this function to combine results from an evaluated test into a matrix and export it to a table, e.g. to import
in other software products. It can be particularily helpful for ET/NRET coded tests, if you want to compare the results of
different valid scoring functions.}
\value{A data.frame.}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\keyword{misc}
\arguments{\item{NR.res,}{\code{NRET.res}, \code{NRETp.res}, \code{ET.res} At least one and up to four objects of class \code{klausuR}. As the names
suggest, take the appropriate element for the scoring policy used to get the results. Usual MC tests must be
given as \code{NR.res}.}
\item{rescale}{Logical, whether ET/NRET scaled results should be rescaled by \code{\link[klausuR]{nret.rescale}}.}
\item{file}{A character string giving a file name to save to. If \code{NULL}, no file will be written.}
\item{csv2}{Logical. If \code{FALSE}, \code{write.csv} will be used instead of \code{write.csv2}.}
\item{encoding}{Encoding of the exported table.}
\item{...}{Additional options for \code{write.csv}/\code{write.csv2}.}
}

