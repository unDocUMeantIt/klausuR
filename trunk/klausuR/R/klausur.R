#' The function \code{klausur} expects an object of class \code{\link[klausuR]{klausuR.answ-class}}\code{\link{data.frame}}, containing some
#' identification data on all subjects and their answers to the test items, a vector with the correct answers, and optionally a vector with
#' marks assigned to the points achieved. It will compute global test results as well as some item analysis (including Cronbach's alpha and
#' discriminatory power of the test items), and anonymous feedback for the test subjects.
#'
#' \code{klausur} automatically parses the variable names to decide \strong{which variables are actual test items}, if they are named according to
#' the given scheme \code{Item###}. To help in constructing a data set with correct variable names one can call the
#' \code{\link[klausuR:klausur.gen]{klausur.gen}} utility to generate an empty data object of a given number of items and test subjects.
#'
#' If you have \strong{items with multiple correct answers} you can easily code these as one single item: All alternatives a subject has marked should be combined
#' to a single value without spaces. The vector with correct answers will have to be coded accordingly, of course. An example: If someone marked the first,
#' third and fourth answer, you would code this as "134". See \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}} for a helpful function to create such an
#' answer vector. Internally \code{klausur} checks for equality of given answers and correct values, that is,
#' it will only give that person a point if the correct answer was coded as "134" as well.
#'
#' In combination with multiple correct answers for certain items you can specify one of six scoring policies via the \code{score}-parameter. By default it is
#' set to \code{"solved"}, which means that one will only get any points for an item if the answer was 100\% correct (that is, all or nothing). If you set it to
#' \code{"partial"} or \code{"liberal"}, as the names may suggest you \strong{allow partially given answers} under the condition that the test subject didn't
#' check more alternatives than there are correct ones (that is, if you checked four alternatives where three correct ones were possible, you're out).
#' In the case of \code{"partial"}, an additional condition is that none of the distractors must be checked, which \code{"liberal"} gracefully allows. An example:
#' If the correct answer is "134" and a subject checked "15", \code{"solved"} will give no point (because "15" is not equal to "134"), as will \code{"partial"}
#' (because "5" is wrong), but \code{"liberal"} will give 1/3 (because "1" is correct).
#'
#' \strong{(Number Right) Elimination Testing}
#'
#' In addition to that, there are also the scoring options \code{score="ET"} (elimination testing), \code{score="NRET"} (number right elimination testing)
#' and \code{score="NR"} (number right). Note that all of the three will disable \code{wght} as well as \code{item.analysis} as of now, and
#' \strong{they need the data in different format} than the other scoring functions! More specific, in contrast to the usual MC procedure ET items are answered
#' by eliminating all alternatives a subject considers \emph{wrong}. Adding to this, in an NRET test subjects are asked to eliminate all wrong alternatives
#' \emph{and} mark the one they consider the correct answer. For both scoring functions, you need to know for each answer alternative whether a subject saw it
#' as right, wrong or was not sure and left it open.
#' In this implementation, these answers are to be coded as a plus sign "+" (right answer), a minus sign "-" (wrong answer) or a zero "0" (missing).
#' If you need to code errors (like both "right" and "wrong" have been marked), you can use the asterisk "*" for these cases.
#' That is, if you have four answer alternatives, a subject thought the second one to be the correct answer and eliminated the rest, you'd have to code
#' this item as \code{"-+--"}. The same is true for the vector of correct answers, of course. NR is actually the usual MC scoring (one point for correct answer)
#' and implemented merely for completeness, e.g. to compare results of different evaluation techniques.
#' 
#' \strong{Marks}
#'
#' The \strong{assigned marks} are expected to be in a certain format as well, as long as you don't want \code{klausur} to suggest them itself (see below).
#' Just create an empty vector to start with (say \code{your.marks <- c()}) and fill it according to the scheme \code{your.marks[<points from>:<points to>] <- <mark>}.
#' Another example: Should one get a 1.7 if in sum 27 to 30 points were achieved, you'd assign these points as indices to the vector with
#' \code{your.marks[27:30] <- "1.7"} (see example section below). It is crucial to assign marks to the whole range of points that can be achieved in the test.
#' On the other hand, it's irrelevant wheter you assign decimal marks as in the example, only integer values, a 15 marks scheme or whatever. The convenience
#' function \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} can assist you in creating such a valid vector.
#'
#' Another way to deal with marks is to let klausuR make a suggestion. That is, if \code{marks="suggest"}, \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}
#' kicks in and takes either the \code{mark.labels} you have defined here or will ask you step by step. See the documentation of that function for details. To see the
#' suggested result in detail, have a look at the slot \code{marks} of the returned object.
#'
#' To calculate Cronbach's alpha and item analysis methods from the package \code{\link[psychometric]{psychometric}} are used.
#'
#' @note \code{klausur} allows some tweaks that are probably not as useful as they seem. For instance, having items with more than one correct answer doesn't
#' necessarily yield more diagnostic information, allowing for those being answered partially adds to that, and binding marks blindly to a normal distribution can
#' give quite unfair test results! In addition, please do \strong{always check a sample of the results} to make sure no errors accurred.
#' 
#' @title A function to evaluate multiple choice tests
#' @usage klausur(answ, corr, marks, mark.labels=NULL, items=NULL,
#' wght=NULL, score="solved", matn=NULL, na.rm=TRUE,
#' cronbach=FALSE, item.analysis=FALSE)
#' @param data An object of class \code{\link[klausuR]{klausuR.answ-class}}.
#' @param marks A vector assigning marks to points achieved (see details). Alternatively, set it to \code{"suggest"} to let
#'		\code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} calculate suggestions under the assumption of normal distribution.
#'		If \code{NULL}, this value must be set in the \code{data} object.
#' @param mark.labels If \code{marks="suggest"}, use these as the marks you want to give.
#' @param items Indices of a subset of variables in \code{data} to be taken as items.
#' @param wght A vector with weights for each item (named also according to \code{Item###}). If \code{NULL}, the value from the \code{data} object
#'		will be used.
#' @param score Specify the scoring policy, must be one of \code{"solved"} (default), \code{"partial"}, \code{"liberal"},
#'		\code{"NR"}, \code{"ET"}, or \code{"NRET"}.
#' @param matn A matriculation number of a subject, to receive detailed results for that subject.
#' @param na.rm Logical, whether cases with NAs should be ignored in \code{data}. Defaults to TRUE.
#' @param cronbach Logical. If TRUE, Cronbach's alpha will be calculated.
#' @param item.analysis Logical. If TRUE, some usual item statistics like difficulty and discriminatory power will be calculated.
#'	If \code{cronbach} is TRUE, too, it will include the alpha values if each item was deleted.
#' @return An object of class \code{\link[klausuR]{klausuR-class}} with the following slots.
#'	\item{results}{A data.frame with global results}
#'	\item{answ}{A data.frame with all given answers}
#'	\item{corr}{A vector with the correct answers}
#'	\item{wght}{A vector with the weights of items}
#'	\item{points}{A data.frame with resulting points given for the answers}
#'	\item{marks}{A vector with assignments of marks to achieved score}
#'	\item{marks.sum}{A more convenient matrix with summary information on the defined marks}
#'	\item{trfls}{A data.frame of TRUE/FALSE values, whether a subject was able to solve an item or not}
#'	\item{anon}{A data.frame for anonymous feedback}
#'	\item{mean}{A table with mean, median and quartiles of the test results}
#'	\item{sd}{Standard deviation of the test results}
#'	\item{cronbach}{Internal consistency, a list of three elements "alpha", "ci" (confidence interval 95\%) and "deleted" (alpha if item was removed)}
#'	\item{item.analysis}{A data.frame with information on difficulty, discriminant power and discriminant factor of all items.}
#'	Not all slots are shown by default (refer to \code{\link[show,klausuR,-method]{show}} and \code{\link[plot,klausuR]{plot}}).
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur.report]{klausur.report}}, \code{\link[klausuR:klausur.compare]{klausur.compare}},
#'  \code{\link[klausuR:klausur.gen]{klausur.gen}}, \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}},
#'  \code{\link[klausuR:klausur.gen.corr]{klausur.gen.corr}},
#'  \code{\link[klausuR:plot]{plot}}, \code{\link[psychometric]{psychometric}}
#' @keywords misc
#' @import psychometric
#' @export
#' @examples
#' data(antworten)
#' 
#' # vector with correct answers:
#' richtig <- c(Item01=3, Item02=2, Item03=2, Item04=2, Item05=4,
#'	Item06=3, Item07=4, Item08=1, Item09=2, Item10=2, Item11=4,
#'	Item12=4, Item13=2, Item14=3, Item15=2, Item16=3, Item17=4,
#'	Item18=4, Item19=3, Item20=5, Item21=3, Item22=3, Item23=1,
#'	Item24=3, Item25=1, Item26=3, Item27=5, Item28=3, Item29=4,
#'	Item30=4, Item31=13, Item32=234)
#' 
#' # vector with assignement of marks:
#' notenschluessel <- c()
#' # scheme of assignments: marks[points_from:to] <- mark
#' notenschluessel[0:12]  <- 5.0
#' notenschluessel[13:15] <- 4.0
#' notenschluessel[16:18] <- 3.7
#' notenschluessel[19:20] <- 3.3
#' notenschluessel[21]    <- 3.0
#' notenschluessel[22]    <- 2.7
#' notenschluessel[23]    <- 2.3
#' notenschluessel[24]    <- 2.0
#' notenschluessel[25:26] <- 1.7
#' notenschluessel[27:29] <- 1.3
#' notenschluessel[30:32] <- 1.0
#' 
#' klsr.obj <- klausur(answ=antworten, corr=richtig, marks=notenschluessel)

klausur <- function(data, marks=NULL, mark.labels=NULL, items=NULL, wght=NULL, score="solved", matn=NULL, na.rm=TRUE, cronbach=TRUE, item.analysis=TRUE){
		## whenever these options/arguments should change, update klausur.mufo() accordingly!

		if(!inherits(data, "klausuR.answ")){
			stop(simpleError("'data' must be of class 'klausuR.answ'!"))
		} else {}

		corr <- data@corr$corr
		if(!is.null(data@corr$corr.key)){
			warning("This test seems to have multiple test forms. Perhaps try klausur.mufo() instead?", .call=FALSE)
		} else {}

		if(is.null(wght)){
			wght <- data@score$wght
		} else {}

		if(is.null(marks)){
			if(is.null(data@score$marks)){
				stop(simpleError("You must give some value for 'marks', either in 'data' or with the klausur() call!"))
			} else {
				marks <- data@score$marks
			}
		} else {}

		## TODO: clean up checks, so cbind is not needed!
		## firstly, check input data an quit if necessary
		# data.check.klausur() is an internal function, defined in klausuR-internal.R
		sane.data <- data.check.klausur(cbind(data@id, data@items), corr, items, na.rm)
		stopifnot(scoring.check.klausur(corr, marks, wght, score))
		answ <- sane.data$answ
		items <- sane.data$items

		### results section
		# set default min.score to zero
		min.score <- 0
		# probably weight items, to calculate the maximum score
		# NRET et al. can't be weighted yet
		if(is.null(wght) | score %in% c("NR", "ET", "NRET")){
			nret.test.chars <- nret.minmax(corr=corr, score=score)
			maxp <- nret.test.chars["maxp"]
			min.score <- nret.test.chars["minp"]
			baseline <- nret.test.chars["baseline"]
			num.alt <- nret.test.chars["num.alt"]
			# for the results, create a vector of 1's by number of items
			wght.results <- rep(1, length(items))
		} else {
			maxp <- sum(wght)
			wght.results <- wght
		}

		# create the TRUE/FALSE-matrix for solved items
		# this will become a point matrix instead if partial results are ok
		if(length(score) == 1 && score %in% c("partial", "liberal", "NR", "ET", "NRET")){
			# weights will be considered here as well
			# if partial answers should be considered
			if(identical(score, "partial")){
				wahr.falsch <- data.frame(sapply(items, function(x) partial(item.answ=answ[x], corr=corr, wght=1, strict=TRUE, mode="percent")))
				ergebnisse  <- data.frame(sapply(items, function(x) partial(item.answ=answ[x], corr=corr, wght=wght[which(items == x)], strict=TRUE, mode="percent")))
			} else {}
			if(identical(score, "liberal")){
				wahr.falsch <- data.frame(sapply(items, function(x) partial(item.answ=answ[x], corr=corr, wght=1, strict=FALSE, mode="percent")))
				ergebnisse  <- data.frame(sapply(items, function(x) partial(item.answ=answ[x], corr=corr, wght=wght[which(items == x)], strict=FALSE, mode="percent")))
			} else {}
			if(score %in% c("NR", "ET", "NRET")){
				wahr.falsch <- data.frame(sapply(items, function(x) {nret.score(answ[[x]], corr=corr[names(answ[x])], score=score, is.true="+", is.false="-", missing="0", err="*",
					num.alt=num.alt, true.false=TRUE)}))
				ergebnisse  <- data.frame(sapply(items, function(x) {nret.score(answ[[x]], corr=corr[names(answ[x])], score=score, is.true="+", is.false="-", missing="0", err="*",
					num.alt=num.alt, true.false=FALSE)}))
			} else {}
			dimnames(wahr.falsch)[[2]] <- names(answ[items])
			dimnames(ergebnisse)[[2]]  <- names(answ[items])
		}
		else {
			wahr.falsch <- data.frame(t(t(answ[,items]) == corr))
			# in case weights were defined, e.g. for items with multiple correct answers, take them into account
			if(!is.null(wght))
				ergebnisse <- data.frame(t(t(wahr.falsch) * wght))
			else
				# we'll add 0 to forcibly convert logical values to numerics
				ergebnisse <- wahr.falsch + 0
		}

		## calculate points
		punkte <- rowSums(ergebnisse)

		## descriptive statistics
		mittel.quart <- summary(punkte)
		stdabw <- sd(punkte, na.rm=TRUE)

		# should marks be suggested or are they given?
		if(length(marks) == 1 && identical(marks, "suggest")){
			warning("Marks are a suggestion, not your definition!", call.=FALSE)
			##
			if(score %in% c("ET", "NRET")){
				# a hack to force the right max. points here
				marks <- klausur.gen.marks(mark.labels=mark.labels, answ=maxp, wght=NULL, suggest=list(mean=mean(punkte), sd=stdabw), minp=baseline)
				} else if(identical(score, "NR")){
					marks <- klausur.gen.marks(mark.labels=mark.labels, answ=maxp, wght=NULL, suggest=list(mean=mean(punkte), sd=stdabw), minp=min.score)
			} else {
				marks <- klausur.gen.marks(mark.labels=mark.labels, answ=answ, wght=wght, suggest=list(mean=mean(punkte), sd=stdabw), minp=min.score)
			}
		}

		# check wheter maximum score matches the assigned marks
		if(length(marks) != maxp){
			warning(paste("Achievable score and marks do not match!\n  Maximum score of test:",maxp,"\n  Maximum score assigned to marks:",length(marks)), call.=FALSE)
		}

		# assign the marks. zero points will result in lowest mark, NA will stay NA
		note <- sapply(punkte, function(x){
				if(is.na(x))
					return(NA) else{}
				if(x > 0)
					note <- marks[x]
				else
					note <- marks[1]
				return(note)
			})

		# create data object with name, mat-nr and global results
		# calls the internal function global.results()
		if(score %in% c("ET", "NRET")){
			ergebnis.daten <- global.results(answ=answ, points=punkte, maxp=maxp, mark=note, minp=baseline)
		} else {
			ergebnis.daten <- global.results(answ=answ, points=punkte, maxp=maxp, mark=note, minp=min.score)
		}
		# if pseudonyms were given, use them for anonymous feedback
		ergebnis.anonym <- anon.results(ergebnis.daten)

		## psychometic quality of the items
		if(!score %in% c("NR", "ET", "NRET")){
			if(isTRUE(cronbach)){
				# calling an internal function which is
				# using alpha() from package "psychometric"
				cron.alpha.list <- calc.cronbach.alpha(na.omit(wahr.falsch))
			} else {
				cron.alpha.list <- list(alpha=NULL, ci=NULL, deleted=NULL)
			}
			if(isTRUE(item.analysis)){
				# calling another internal function which is also
				# using alpha() from package "psychometric"
				item.analyse <- calc.item.analysis(wahr.falsch, cron.alpha.list)
			} else {
				item.analyse <- data.frame(NA)
			}
		} else {
			cron.alpha.list <- list(alpha=NULL, ci=NULL, deleted=NULL)
			item.analyse <- data.frame(NA)
		}

		## compose the resulting object
		# here we make a copy of the TRUE/FALSE matrix, to be able to check each result individually
		wahrfalsch.daten <- cbind(MatrNo=answ$MatrNo, wahr.falsch)
		# the same way make a copy of the matrix with resulting points, especially interesting if items were weighted
		ergebnisse.daten <- cbind(MatrNo=answ$MatrNo, ergebnisse)
		# and we make a copy with given answers as well
		antwort.daten <- cbind(MatrNo=answ$MatrNo, answ[,items])
		# use the internal marks.summary() function to create convenient information on the mark definitions
		marks.info <- marks.summary(marks, minp=min.score)
		# here we finally take all the partial results an knit them together into one object
		alle.ergebnisse <- new("klausuR",
					results=ergebnis.daten,
					answ=antwort.daten,
					corr=corr,
					wght=wght.results,
					points=ergebnisse.daten,
					marks=marks,
					marks.sum=marks.info,
					trfls=wahrfalsch.daten,
					anon=ergebnis.anonym,
					mean=mittel.quart,
					sd=stdabw,
					cronbach=cron.alpha.list,
					item.analysis=item.analyse,
					misc=data@misc)

		## output options
		# return all data or just for one matriculation number?
		if(!is.null(matn)){
			if(!sum(answ$MatrNo == matn) == 1){
			stop(simpleError("The given matriculation number is not defined!"), call.=FALSE)
			}
			pers.ergebnisse <- ergebnis.daten[ergebnis.daten$MatrNo == matn,]
			items.solved <- wahrfalsch.daten[wahrfalsch.daten$MatrNo == matn, -1]
			rownames(items.solved) <- "Solved"
			ausgabe <- list(Results=pers.ergebnisse, SolvedItems=t(items.solved))
		}
		else
				# if matn is not set, return the default result object
				ausgabe <- alle.ergebnisse

		## ausgabe
		return(ausgabe)
		}
