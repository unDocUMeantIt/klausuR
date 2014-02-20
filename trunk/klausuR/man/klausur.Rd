\name{klausur}
\alias{klausur}
\title{Evaluate multiple choice tests}
\usage{
  klausur(data, marks = NULL, mark.labels = NULL,
    items = NULL, wght = NULL, score = "solved",
    matn = NULL, na.rm = TRUE, cronbach = TRUE,
    item.analysis = TRUE, sort.by = "Name", maxp = NULL)
}
\arguments{
  \item{data}{An object of class
  \code{\link[klausuR]{klausuR.answ-class}}.}

  \item{marks}{A vector assigning marks to points achieved
  (see details). Alternatively, set it to \code{"suggest"}
  to let
  \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}
  calculate suggestions under the assumption of normal
  distribution.  If \code{NULL}, this value must be set in
  the \code{data} object.}

  \item{mark.labels}{If \code{marks="suggest"}, use these
  as the marks you want to give.}

  \item{items}{Indices of a subset of variables in
  \code{data} to be taken as items.}

  \item{wght}{A vector with weights for each item (named
  also according to \code{Item###}). If \code{NULL}, the
  value from the \code{data} object will be used.}

  \item{score}{Specify the scoring policy, must be one of
  \code{"solved"} (default), \code{"partial"},
  \code{"liberal"}, \code{"NR"}, \code{"ET"},
  \code{"NRET"}, or \code{"NRET+"}.}

  \item{matn}{A matriculation number of a subject, to
  receive detailed results for that subject.}

  \item{na.rm}{Logical, whether cases with NAs should be
  ignored in \code{data}. Defaults to TRUE.}

  \item{cronbach}{Logical. If TRUE, Cronbach's alpha will
  be calculated.}

  \item{item.analysis}{Logical. If TRUE, some usual item
  statistics like difficulty and discriminatory power will
  be calculated.}

  \item{sort.by}{A character string naming the variable to
  sort the results by. Set to \code{c()} to skip any
  re-ordering.  If \code{cronbach} is TRUE, too, it will
  include the alpha values if each item was deleted.}

  \item{maxp}{Optional numeric value, if set will be forced
  as the maximum number of points achievable. This should
  actually not be needed, if your test has no strange
  errors. But if for example it later turns out you need to
  adjust one item because it has two instead of one correct
  answers, this option can become handy in combination with
  "partial" scoring and item weights.}
}
\value{
  An object of class \code{\link[klausuR]{klausuR-class}}
  with the following slots.  \item{results}{A data.frame
  with global results} \item{answ}{A data.frame with all
  given answers} \item{corr}{A vector with the correct
  answers} \item{wght}{A vector with the weights of items}
  \item{points}{A data.frame with resulting points given
  for the answers} \item{marks}{A vector with assignments
  of marks to achieved score} \item{marks.sum}{A more
  convenient matrix with summary information on the defined
  marks} \item{trfls}{A data.frame of TRUE/FALSE values,
  whether a subject was able to solve an item or not}
  \item{anon}{A data.frame for anonymous feedback}
  \item{mean}{A table with mean, median and quartiles of
  the test results} \item{sd}{Standard deviation of the
  test results} \item{cronbach}{Internal consistency, a
  list of three elements "alpha", "ci" (confidence interval
  95\%) and "deleted" (alpha if item was removed)}
  \item{item.analysis}{A data.frame with information on
  difficulty, discriminant power, discriminant factor and
  Lienert's selection index of all items.}
  \item{misc}{Anything that was stored in the \code{misc}
  slot of the input data.} Not all slots are shown by
  default (refer to \code{\link[klausuR:show]{show}} and
  \code{\link[klausuR:plot]{plot}}).
}
\description{
  The function \code{klausur} expects an object of class
  \code{\link[klausuR]{klausuR.answ-class}}, containing
  some identification data on all subjects and their
  answers to the test items, a vector with the correct
  answers, and optionally a vector with marks assigned to
  the points achieved. It will compute global test results
  as well as some item analysis (including Cronbach's
  alpha, discriminatory power and Lienert's selection index
  of the test items), and anonymous feedback for the test
  subjects.
}
\details{
  For details on the ecpected data structure refer to
  \code{\link[klausuR:klausur.data]{klausur.data}},

  \strong{Scoring functions}

  In combination with multiple (correct) answers for
  certain items you can specify one of six scoring policies
  via the \code{score}-parameter. If you set it to
  something othe than \code{"solved"}, as the names may
  suggest you \strong{allow partially given answers} under
  the condition that the test subject didn't check more
  alternatives than there are correct ones (that is, if you
  checked four alternatives where three correct ones were
  possible, you're out): \itemize{ \item{\code{"solved"}}
  {Multiple Choice: Check all correct alternatives. This is
  the default, which means that one will only get any
  points for an item if the answer was 100\% correct (that
  is, all or nothing).} \item{\code{"partial"}} {Multiple
  Choice: Check all correct alternatives, allow partially
  given answers, but none of the distractors must be
  checked.} \item{\code{"liberal"}} {Multiple Choice: Check
  all correct alternatives, allow partially given answers,
  even distractors can be checked.} \item{\code{"ET"}}
  {Elimination Testing: In contrast to the usual MC
  procedure, eliminate/strike all \emph{wrong}
  alternatives.} \item{\code{"NRET"}} {Number Right
  Elimination Testing: Like ET+MC, eliminate/strike all
  \emph{wrong} alternatives \emph{and} check the correct
  one.} \item{\code{"NRET+"}} {Number Right Elimination
  Testing, more strict: Like NRET, but if more alternatives
  are checked right than there are right anwers, it will
  automatically yield to 0 points for that item.}
  \item{\code{"NR"}} {Number Right: The usual MC scoring,
  but works with ET/NRET data. It was implemented for
  completeness, e.g. to compare results of different
  scoring techniques.} }

  An example for \code{"solved"}, \code{"partial"} and
  \code{"liberal"}: If the correct answer is "134" and a
  subject checked "15", \code{"solved"} will give no point
  (because "15" is not equal to "134"), as will
  \code{"partial"} (because "5" is wrong), but
  \code{"liberal"} will give 1/3 (because "1" is correct).

  \strong{(Number Right) Elimination Testing}

  Note that \code{"ET"}, \code{"NRET"}/\code{"NRET+"} and
  \code{"NR"} will disable \code{wght} as of now, and
  \strong{need the data in different format} than the other
  scoring functions (see
  \code{\link[klausuR:klausur.data]{klausur.data}} for
  details). \code{klausur} will evaluate each answer
  individually and sum up the points for each item. The
  alternative-wise evaluations will be documented in the
  \code{trfls} slot of the results. Therefore, in these
  cases that matrix is not boolean, but more complex. For
  each item and each subject, a character string represents
  the evaluated answer alternatives, with the following
  elements: \itemize{ \item{\code{P}} {True positive:
  Alternative was checked as right and is right.
  \emph{Points: +1 (+ constant)}} \item{\code{p}} {False
  positive: Alternative was checked as right but is wrong.
  \emph{Points: 0 (+ constant)}} \item{\code{N}} {True
  negative: Alternative was checked as wrong and is wrong.
  \emph{Points: +1 (+ constant)}} \item{\code{n}} {False
  negative: Alternative was checked as wrong but is right.
  \emph{Points: -(alternatives-1) (+ constant)}}
  \item{\code{0}} {Missing: Alternative wasn't checked at
  all. \emph{Points: 0 (+ constant)}} \item{\code{*}}
  {Error: Alternative was checked both wrong and right.
  \emph{Points NRET+: 0 (+ constant); NR scores 1 point if
  this was the correct alternative, ET 1 point if it hit a
  wrong one, and NRET sums up the points for both the
  positive and negative answer (all + constant)}} } An
  example: If we have an item with four alternatives, and
  the third one is right (i.e., "\code{--+-}"), and a test
  subject considered the first alternative to be correct
  and eliminated all others (i.e., "\code{+---}"), it would
  be evaluated as "\code{pNnN}", that is 0+1-3+1=-1 point,
  not considering the constant. As you can see, it would be
  possible to end up with a negative sum of points. If you
  consider how in the end a mark will be assigned to the
  achieved points, this would be a problem, because a
  vactor cannot have negative indices. To circumvent this
  issue, klausuR automatically adds a constant to all
  results, so that the worst possible result is not
  negative but 0. This constant is simply (alternatives-1),
  i.e. 3 for the example. In other words, if our test had
  10 such items, the results minus 30 would be equivalent
  to scoring without that constant. You can use
  \code{\link[klausuR:nret.rescale]{nret.rescale}} to
  remove the constant from the results afterwards.

  \strong{Marks}

  The \strong{assigned marks} are expected to be in a
  certain format as well (see
  \code{\link[klausuR:klausur.data]{klausur.data}} for
  details), as long as you don't want \code{klausur} to
  suggest them itself. If you want to let klausuR make a
  suggestion, set \code{marks="suggest"}, and
  \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}
  kicks in and takes either the \code{mark.labels} you have
  defined here or will ask you step by step. See the
  documentation of that function for details. To see the
  suggested result in detail, have a look at the slot
  \code{marks} of the returned object.

  To calculate Cronbach's alpha and item analysis methods
  from the package \code{\link[psychometric]{psychometric}}
  are used. Lienert's selction index ("Selektionskennwert")
  aims to consider both discriminatory power (correlation
  of an item with the test results) and difficulty to
  determine the quality of an item. It is defined as
  \deqn{S = \frac{r_{it}}{2 \times{} \sqrt{Difficulty
  \times{} (1-Difficulty)}}}
}
\note{
  \code{klausur} allows some tweaks that are probably not
  as useful as they seem. For instance, having items with
  more than one correct answer doesn't necessarily yield
  more diagnostic information, allowing for those being
  answered partially adds to that, and binding marks
  blindly to a normal distribution can give quite unfair
  test results! In addition, please do \strong{always check
  a sample of the results} to make sure no errors accurred.
}
\examples{
data(antworten)

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
klsr.obj <- klausur(data.obj)

############################
	# example for an NRET test
############################
# load sampla data in SPSS format
data(spss.data)
# define correct answers
spss.corr <- c(
		item01=2, item02=3, item03=3, item04=3, item05=2,
		item06=2, item07=3, item08=1, item09=1, item10=2)

# convert into klausuR type coding
klausuR.data <- nret.translator(spss.data, spss="in")
klausuR.corr <- nret.translator(spss.corr, spss="in", corr=TRUE,
  num.alt=3, spss.prefix=c(corr="item"))
# now create the data object; "Nickname" must be renamed
data.obj <- klausur.data(answ=klausuR.data, corr=klausuR.corr,
  rename=c(Pseudonym="Nickname"))

	# finally, the test can be evaluated, using the scoring functions available
NRET.results <- klausur(data.obj, marks="suggest", mark.labels=11, score="NRET")
NRETplus.results <- klausur(data.obj, marks="suggest", mark.labels=11, score="NRET+")
NR.results <- klausur(data.obj, marks="suggest", mark.labels=11, score="NR")
ET.results <- klausur(data.obj, marks="suggest", mark.labels=11, score="ET")
}
\author{
  m.eik michalke \email{meik.michalke@uni-duesseldorf.de}
}
\seealso{
  \code{\link[klausuR:klausur.data]{klausur.data}},
  \code{\link[klausuR:klausur.report]{klausur.report}},
  \code{\link[klausuR:compare]{compare}},
  \code{\link[klausuR:klausur.gen]{klausur.gen}},
  \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}},
  \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}},
  \code{\link[klausuR:plot]{plot}},
  \code{\link[psychometric]{psychometric}}
}
\keyword{misc}

