\name{klausuR-class}
\alias{klausuR,-class
-class}
\alias{klausuR,-class}
\title{S4 class klausuR}
\description{Class klausuR}
\details{This class is used for objects that are returned by \code{\link[klausuR:klausur]{klausur}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{results}:}{(\code{\link{data.frame}}) A data.frame with global results}\item{\code{answ}:}{(\code{\link{data.frame}}) A data.frame with all given answers}\item{\code{corr}:}{(\code{\link{vector}}) A vector with the correct answers}\item{\code{wght}:}{(\code{\link{vector}}) A vector with the weights of items}\item{\code{points}:}{(\code{\link{data.frame}}) A data.frame with resulting points given for the answers}\item{\code{marks}:}{(\code{\link{vector}}) A vector with assignments of marks to achieved score}\item{\code{marks.sum}:}{(\code{\link{matrix}}) A more convenient matrix with summary information on the defined marks}\item{\code{trfls}:}{(\code{\link{data.frame}}) A data.frame of TRUE/FALSE values, whether a subject was able to solve an item or not}\item{\code{anon}:}{(\code{\link{data.frame}}) A data.frame for anonymous feedback}\item{\code{mean}:}{(\code{\link{table}}) A table with mean, median and quartiles of the test results}\item{\code{sd}:}{(\code{\link{numeric}}) Standard deviation of the test results}\item{\code{cronbach}:}{(\code{\link{list}}) Internal consistency, a list of three elements "alpha", "ci" (confidence interval 95\%) and "deleted" (alpha if item was removed)}\item{\code{item.analysis}:}{(\code{\link{data.frame}}) A data.frame with information on difficulty, discriminant power and discriminant factor of all items.}\item{\code{test}:}{(\code{\link{klausuR.test}}) Currently an empty placeholder. Planned to hold the actual test items in future releases.}\item{\code{misc}:}{(\code{\link{data.frame}}) Anything that was stored in the \code{misc} slot of the input data.}}}

