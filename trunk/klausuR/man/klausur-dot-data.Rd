\name{klausur.data}
\alias{klausur.data}
\title{A function to create data objects with given and correct answers to a test.}
\usage{klausur.data(answ, corr, items, marks, wght, corr.key, rename=c(), dummies=c(),
    disc.misc=FALSE, na.rm=TRUE)
}
\description{A function to create data objects with given and correct answers to a test.}
\details{\code{klausur.data} automatically parses the variable names in \code{answ}to decide \strong{which variables are actual test items},
if they are named according to the given scheme \code{Item###}. To help in constructing a data.frame with correct column names one can call the
\code{\link[klausuR:klausur.gen]{klausur.gen}} utility to generate an empty data object of a given number of items and test subjects.

If you have \strong{items with multiple correct answers} you can easily code these as one single item: All alternatives a subject has marked should be combined
to a single value without spaces. The vector with correct answers will have to be coded accordingly, of course. An example: If someone marked the first,
third and fourth answer, you would code this as "134". See \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}} for a helpful function to create such an
answer vector. Internally \code{klausur} checks for equality of given answers and correct values, that is,
it will only give that person a point if the correct answer was coded as "134" as well.

\strong{Data for (Number Right) Elimination Testing}

If your test is to be evaluated according to elimination testing (ET), number right elimination testing (NRET) or number right (NR, which is actually
multiple choice) scoring, the data has to be in a different format: In contrast to the usual MC procedure, ET items are answered
by eliminating all alternatives a subject considers \emph{wrong}; in an NRET test subjects are asked to eliminate all wrong alternatives
\emph{and} mark the one they consider the correct answer. That is, for both scoring functions, you need to know for each answer alternative whether
a subject saw it as right, wrong or was not sure and left it open.

In this implementation, these answers are to be coded as a plus sign "\code{+}" (right answer), a minus sign "\code{-}" (wrong answer) or a zero
"\code{0}" (missing). If you need to code errors (like both "right" and "wrong" have been marked),use the asterisk "\code{*}" for these cases.
All answers to \strong{one item} belong into \strong{one column}. E.g., if you have four answer alternatives, a subject thought the second one to be the correct
answer and eliminated the rest, you'd have to code this item as "\code{-+--}". The same is true for the vector of correct answers, of course.

\strong{Marks}

The \strong{assigned marks} are expected to be in a certain format as well, as long as you don't want \code{klausur} to suggest them itself.
Just create an empty vector to start with (say \code{your.marks <- c()}) and fill it according to the scheme \code{your.marks[<points from>:<points to>] <- <mark>}.
For example: Should one get a 1.7 if in sum 27 to 30 points were achieved, you'd assign these points as indices to the vector with
\code{your.marks[27:30] <- "1.7"} (see example section below). It is crucial to assign marks to the whole range of points that can be achieved in the test.
On the other hand, it's irrelevant wheter you assign decimal marks as in the example, only integer values, a 15 marks scheme or whatever. The convenience
function \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} can assist you in creating such a valid vector.}
\value{An object of class \code{\link[klausuR]{klausuR.answ-class}}.}
\arguments{\item{answ}{A \code{\link{data.frame}} which has to include at least these variables:
\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, as well as \code{Pseudonym} (optional)
and variables for the answered items (according to the scheme \code{Item###},
where ### is a number with leading zeros, if needed).}
\item{corr}{A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).}
\item{items}{Indices of a subset of variables in \code{answ} to be taken as items.}
\item{marks}{A vector assigning marks to points achieved (see details). Leave \code{NULL} if not available.}
\item{wght}{A vector with weights for each item (named also according to \code{Item###}). Leave \code{NULL} if not available.}
\item{corr.key}{If test has several test forms: A data.frame or matrix indicating the positions of all items (columns) in all
forms (rows). Must have a column called \code{Form} (like \code{answ}), and the item columns must follow the explained name
scheme \code{Item###}. \code{NULL} if not needed.}
\item{rename}{A named vector defining if variables in \code{answ} need to be renamed into the klausuR name scheme. Accepts elements
named \code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, \code{Pseudonym} and \code{Form}. The values of these elements
represent the variable names of the input data.}
\item{dummies}{A vector of dummy variables to be created, e.g. if you don't need/want actual data in the \code{id} slot.
Can include \code{"No"}, \code{"Name"}, \code{"FirstName"}, \code{"MatrNo"} and \code{"Pseudonym"}. Columns will just be filled
with increasing integers.}
\item{disc.misc}{Logical. If \code{TRUE}, left over columns from \code{answ} will not be stored in slot \code{misc} but silently discarded.}
\item{na.rm}{Logical, whether cases with NAs should be ignored in \code{answ}. Defaults to TRUE.}
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

# now combine all test data into one object of class klausur.answ
data.obj <- klausur.data(answ=antworten, corr=richtig, marks=notenschluessel)

# if that went well, get the test results
klsr.obj <- klausur(data.obj)}

