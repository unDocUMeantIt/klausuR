\name{klausur.mufo}
\alias{klausur.mufo}
\title{A function to evaluate multiple choice tests}
\usage{klausur.mufo(answ, corr, corr.key, marks, mark.labels=NULL,
items = NULL, wght = NULL, score="solved", matn = NULL,
na.replace = NULL, cronbach=FALSE, item.analysis=FALSE)}
\description{This function can be used to evaluate tests that have several test forms. Please be aware that its results only make sense
if each test form uses the same items, only in a different order.}
\details{Firstly, \code{klausur.mufo} will compute partial results for each parallel form, and in the end combine these to global
results. Cronbach alpha and item analysis will be calculated for all subjects accordingly, therefore the test items of
all tests will be re-ordered to fit the order of the first given test form (this does not apply to the partial results).

The parameters are mostly the same as those for \code{\link[klausuR:klausur]{klausur}}. One new parameter \code{corr.key}
is needed to communicate the order of items in each test form, and the data.frame \code{answ} needs one additional variable
called \code{Form}.

An example: You have prepared a test in two different parallel forms "A" an "B", So in addition to the variables in \code{answ}
you need to create a variable called \code{Form}, to document which test subject was given which test form. Since form "B" holds the same
items as form "A", only in a different order, we only need to define these positions and we're done. Therefore \code{corr.key} must
be a matrix or data.frame, again with a column called "Form", one for each item, and one row of data for each test form. That is, you'd need
one row for test form "A" and one for test form "B", giving an index for each item where it is placed in the form. For "A" this is
simply ascending numbers from 1 to how many questions you asked, but for row "B" each number indicates at which position an item
of "A" is to be found. See the example below.}
\value{An object of class \code{\link[klausuR.mult]{klausuR.mult-class}} with the following slots.
\item{forms}{A character vector naming all test forms}
\item{results.part}{A list of object of class klausuR, holding all partial results}
\item{results.glob}{An object of class klausuR with the global results}
Not all slots are shown by default (refer to \code{\link[show,klausuR.mult-method]{show}}).}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\seealso{\code{\link[klausuR:klausur]{klausur}}}
\keyword{misc}
\arguments{\item{answ}{A \code{\link{data.frame}} which has to include at least these variables:
\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo} and \code{Form}, as well as \code{Pseudonym} (optional)
and variables for the ansered items (according to the scheme \code{Item###},
where ### is a number with leading zeros, if needed).}
\item{corr}{A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).}
\item{corr.key}{A data.frame or matrix indicating the positions of all items (columns) in all forms (rows). Must have
a column called \code{Form} (like \code{answ}), and the item columns must follow the explained name scheme \code{Item###}.}
\item{marks}{A vector assigning marks to points achieved (see details). Alternatively, set it to \code{"suggest"} to let
\code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} calculate suggestions under the assumption of normal distribution.}
\item{mark.labels}{If \code{marks="suggest"}, use these as the marks you want to give.}
\item{items}{Indices of a subset of variables in \code{answ} to be taken as items.}
\item{wght}{A vector with weights for each item (named also according to \code{Item###}).}
\item{score}{Specify the scoring policy, must be one of \code{"solved"} (default), \code{"partial"} or \code{"liberal"}.}
\item{matn}{A matriculation number of a subject, to receive detailed results for that subject.}
\item{na.replace}{A single value to replace NAs with in \code{answ}.}
\item{cronbach}{Logical. If TRUE, Cronbach's alpha will be calculated.}
\item{item.analysis}{Logical. If TRUE, some usual item statistics like difficulty and discriminatory power will be calculated.
If \code{cronbach} is TRUE, too, it will include the alpha values if each item was deleted.}
}
\examples{# this will create the data.frame "antworten.mufo"
# and the matrix "corr.key"
data(antworten.mufo)

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

# expect some warnings here, because some items have no variance
# in their subtest results, hence item analysis fails on them
klsr.mufo.obj <- klausur(answ=antworten.mufo, corr=richtig,
corr.key=corr.key, marks=notenschluessel)}

