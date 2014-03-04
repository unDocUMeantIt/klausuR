\docType{class}
\name{klausuR.answ,-class}
\alias{klausuR.answ,-class}
\alias{klausuR.answ-class}
\title{Class klausuR.answ}
\description{
This class is used for objects needed by
\code{\link[klausuR:klausur]{klausur}}. They contain all
relevant data to calculate test results.
}
\section{Slots}{

\describe{
\item{\code{corr}}{Contains two elements:
   \itemize{
     \item{\code{corr}} {The correct answers to each item.}
     \item{\code{corr.key}} {An optional data.frame or matrix for test with multiple test forms,
      indicating the positions
       of all items (columns) in all forms (rows). Must have a column called \code{Form} (like in \code{id}),
      and the
       item columns must follow the usual name scheme \code{Item###}. \code{NULL} if not needed.}
   }}

\item{\code{id}}{Contains the columns \code{No}, \code{Name}, \code{FirstName},
      \code{MatrNo}, \code{Pseudonym} and \code{Form}.}

\item{\code{items}}{Contains a copy of \code{id$MatrNo} and all answers to the test items (one item per column).}

\item{\code{score}}{Contains three elements:
   \itemize{
     \item{\code{marks}} {The assigned marks for achieved points (\code{NULL} if none)}
     \item{\code{wght}} {Weights for each item (\code{NULL} if none)}
     \item{\code{maxp}} {Optional,
      to force a certain maximum points value (\code{NULL} if none)}
   }}

\item{\code{test}}{Currently an empty placeholder. Planned to hold the actual test items in future releases.}

\item{\code{misc}}{Any additional data you'd like to be stored along with \code{id} and \code{items},
   e.g. table data from/for other software products. Won't be used for anything.}
}}
\note{
The slots \code{}, \code{id}, \code{items} and \code{misc},
must have the same number of rows and contain copies of the
colum \code{MatrNo} for identification.
}
\keyword{classes}

