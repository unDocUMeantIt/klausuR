\name{klausur}
\alias{klausur}
\title{A function to evaluate multiple choice tests}
\usage{klausur(answ, corr, marks, mark.labels=NULL, items=NULL,
wght=NULL, score="solved", matn=NULL, na.rm=TRUE,
cronbach=FALSE, item.analysis=FALSE)}
\description{The function \code{klausur} expects a \code{\link{data.frame}} containing some identification data on all subjects and their answers to the test items,
a vector with the correct answers, and a vector with marks assigned to the points achieved. It will compute global test results as well as
some item analysis (including Cronbach's alpha and discriminatory power of the test items), and anonymous feedback for the test subjects.}
\details{\code{klausur} automatically parses the variable names to decide \strong{which variables are actual test items}, if they are named according to
the given scheme \code{Item###}. To help in constructing a data set with correct variable names one can call the
\code{\link[klausuR:klausur.gen]{klausur.gen}} utility to generate an empty data object of a given number of items and test subjects.

If you have \strong{items with multiple correct answers} you can easily code these as one single item: All alternatives a subject has marked should be combined
to a single value without spaces. The vector with correct answers will have to be coded accordingly, of course. An example: If someone marked the first,
third and fourth answer, you would code this as "134". See \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}} for a helpful function to create such an
answer vector. Internally \code{klausur} checks for equality of given answers and correct values, that is,
it will only give that person a point if the correct answer was coded as "134" as well.

In combination with multiple correct answers for certain items you can specify one of three scoring policies via the \code{score}-parameter. By default it is
set to \code{"solved"}, which means that one will only get any points for an item if the answer was 100\% correct (that is, all or nothing). If you set it to
\code{"partial"} or \code{"liberal"}, as the names may suggest you \strong{allow partially given answers} under the condition that the test subject didn't
check more alternatives than there are correct ones (that is, if you checked four alternatives where three correct ones were possible, you're out).
In the case of \code{"partial"}, an additional condition is that none of the distractors must be checked, which \code{"liberal"} gracefully allows. An example:
If the correct answer is "134" and a subject checked "15", \code{"solved"} will give no point (because "15" is not equal to "134"), as will \code{"partial"}
(because "5" is wrong), but \code{"liberal"} will give 1/3 (because "1" is correct).

The \strong{assigned marks} are expected to be in a certain format as well, as long as you don't want \code{klausur} to suggest them itself (see below).
Just create an empty vector to start with (say \code{your.marks <- c()}) and fill it according to the scheme \code{your.marks[<points from>:<points to>] <- <mark>}.
Another example: Should one get a 1.7 if in sum 27 to 30 points were achieved, you'd assign these points as indices to the vector with
\code{your.marks[27:30] <- "1.7"} (see example section below). It is crucial to assign marks to the whole range of points that can be achieved in the test.
On the other hand, it's irrelevant wheter you assign decimal marks as in the example, only integer values, a 15 marks scheme or whatever. The convenience
function \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} can assist you in creating such a valid vector.

Another way to deal with marks is to let klausuR make a suggestion. That is, if \code{marks="suggest"}, \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}
kicks in and takes either the \code{mark.labels} you have defined here or will ask you step by step. See the documentation of that function for details. To see the
suggested result in detail, have a look at the slot \code{marks} of the returned object.

To calculate Cronbach's alpha and item analysis methods from the package \code{\link[psychometric]{psychometric}} are used.

Note: \code{klausur} allows some tweaks that are probably not as useful as they seem. For instance, having items with more than one correct answer doesn't
necessarily yield more diagnostic information, allowing for those being answered partially adds to that, and binding marks blindly to a normal distribution can
give quite unfair test results! In addition, please do \strong{always check a sample of the results} to make sure no errors accurred.}
\value{An object of class \code{\link[klausuR]{klausuR-class}} with the following slots.
\item{results}{A data.frame with global results}
\item{answ}{A data.frame with all given answers}
\item{corr}{A vector with the correct answers}
\item{wght}{A vector with the weights of items}
\item{points}{A data.frame with resulting points given for the answers}
\item{marks}{A vector with assignments of marks to achieved score}
\item{marks.sum}{A more convenient matrix with summary information on the defined marks}
\item{trfls}{A data.frame of TRUE/FALSE values, whether a subject was able to solve an item or not}
\item{anon}{A data.frame for anonymous feedback}
\item{mean}{A table with mean, median and quartiles of the test results}
\item{sd}{Standard deviation of the test results}
\item{cronbach}{Internal consistency, a list of three elements "alpha", "ci" (confidence interval 95\%) and "deleted" (alpha if item was removed)}
\item{item.analysis}{A data.frame with information on difficulty, discriminant power and discriminant factor of all items.}
Not all slots are shown by default (refer to \code{\link[show,klausuR,-method]{show}} and \code{\link[plot,klausuR]{plot}}).}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\seealso{\code{\link[klausuR:klausur.report]{klausur.report}}, \code{\link[klausuR:klausur.compare]{klausur.compare}},
\code{\link[klausuR:klausur.gen]{klausur.gen}}, \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}},
\code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}},
\code{\link[klausuR:plot]{plot}}, \code{\link[psychometric]{psychometric}}}
\keyword{misc}
\arguments{\item{answ}{A \code{\link{data.frame}} which has to include at least these variables:
\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, as well as \code{Pseudonym} (optional)
and variables for the answered items (according to the scheme \code{Item###},
where ### is a number with leading zeros, if needed).}
\item{corr}{A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).}
\item{marks}{A vector assigning marks to points achieved (see details). Alternatively, set it to \code{"suggest"} to let
\code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} calculate suggestions under the assumption of normal distribution.}
\item{mark.labels}{If \code{marks="suggest"}, use these as the marks you want to give.}
\item{items}{Indices of a subset of variables in \code{answ} to be taken as items.}
\item{wght}{A vector with weights for each item (named also according to \code{Item###}).}
\item{score}{Specify the scoring policy, must be one of \code{"solved"} (default), \code{"partial"} or \code{"liberal"}.}
\item{matn}{A matriculation number of a subject, to receive detailed results for that subject.}
\item{na.rm}{Logical, whether cases with NAs should be ignored in \code{answ}. Defaults to TRUE.}
\item{cronbach}{Logical. If TRUE, Cronbach's alpha will be calculated.}
\item{item.analysis}{Logical. If TRUE, some usual item statistics like difficulty and discriminatory power will be calculated.
If \code{cronbach} is TRUE, too, it will include the alpha values if each item was deleted.}
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

klsr.obj <- klausur(answ=antworten, corr=richtig, marks=notenschluessel)}

