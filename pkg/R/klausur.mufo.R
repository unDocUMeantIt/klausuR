#' This function can be used to evaluate tests that have several test forms. Please be aware that its results only make sense
#' if each test form uses the same items, only in a different order.
#'
#' Firstly, \code{klausur.mufo} will compute partial results for each parallel form, and in the end combine these to global
#' results. Cronbach alpha and item analysis will be calculated for all subjects accordingly, therefore the test items of
#' all tests will be re-ordered to fit the order of the first given test form (this does not apply to the partial results).
#'
#' The parameters are mostly the same as those for \code{\link[klausuR:klausur]{klausur}}. One new parameter \code{corr.key}
#' is needed to communicate the order of items in each test form, and the data.frame \code{answ} needs one additional variable
#' called \code{Form}.
#' 
#' An example: You have prepared a test in two different parallel forms "A" an "B", So in addition to the variables in \code{answ}
#' you need to create a variable called \code{Form}, to document which test subject was given which test form. Since form "B" holds the same
#' items as form "A", only in a different order, we only need to define these positions and we're done. Therefore \code{corr.key} must
#' be a matrix or data.frame, again with a column called "Form", one for each item, and one row of data for each test form. That is, you'd need
#' one row for test form "A" and one for test form "B", giving an index for each item where it is placed in the form. For "A" this is
#' simply ascending numbers from 1 to how many questions you asked, but for row "B" each number indicates at which position an item
#' of "A" is to be found. See the example below.
#' 
#' @title A function to evaluate multiple choice tests
#' @usage klausur.mufo(answ, corr, corr.key, marks, mark.labels=NULL,
#' items = NULL, wght = NULL, score="solved", matn = NULL,
#' na.replace = NULL, cronbach=FALSE, item.analysis=FALSE)
#' @param answ A \code{\link{data.frame}} which has to include at least these variables:
#'	\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo} and \code{Form}, as well as \code{Pseudonym} (optional)
#'	and variables for the ansered items (according to the scheme \code{Item###},
#'	where ### is a number with leading zeros, if needed).
#' @param corr A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).
#' @param corr.key A data.frame or matrix indicating the positions of all items (columns) in all forms (rows). Must have
#'	a column called \code{Form} (like \code{answ}), and the item columns must follow the explained name scheme \code{Item###}.
#' @param marks A vector assigning marks to points achieved (see details). Alternatively, set it to \code{"suggest"} to let
#'	\code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}} calculate suggestions under the assumption of normal distribution.
#' @param mark.labels If \code{marks="suggest"}, use these as the marks you want to give.
#' @param items Indices of a subset of variables in \code{answ} to be taken as items.
#' @param wght A vector with weights for each item (named also according to \code{Item###}).
#' @param score Specify the scoring policy, must be one of \code{"solved"} (default), \code{"partial"} or \code{"liberal"}.
#' @param matn A matriculation number of a subject, to receive detailed results for that subject.
#' @param na.replace A single value to replace NAs with in \code{answ}.
#' @param cronbach Logical. If TRUE, Cronbach's alpha will be calculated.
#' @param item.analysis Logical. If TRUE, some usual item statistics like difficulty and discriminatory power will be calculated.
#'	If \code{cronbach} is TRUE, too, it will include the alpha values if each item was deleted.
#' @return An object of class \code{\link[klausuR.mult]{klausuR.mult-class}} with the following slots.
#'	\item{forms}{A character vector naming all test forms}
#'	\item{results.part}{A list of object of class klausuR, holding all partial results}
#'	\item{results.glob}{An object of class klausuR with the global results}
#'	Not all slots are shown by default (refer to \code{\link[show,klausuR.mult-method]{show}}).
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}
#' @keywords misc
#' @import psychometric
#' @export
#' @examples
#' # this will create the data.frame "antworten.mufo"
#' # and the matrix "corr.key"
#' data(antworten.mufo)
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
#' # expect some warnings here, because some items have no variance
#' # in their subtest results, hence item analysis fails on them
#' klsr.mufo.obj <- klausur(answ=antworten.mufo, corr=richtig,
#'      corr.key=corr.key, marks=notenschluessel)

klausur.mufo <- function(answ, corr, corr.key, marks, mark.labels=NULL, items=NULL, wght=NULL, score="solved", matn=NULL, na.replace=NULL, cronbach=TRUE, item.analysis=TRUE){

    ## first we'll check if the data is sane
    # are we given any parallel forms at all?
    if(is.null(answ[["Form"]]) || is.null(dim(corr.key)) || is.null(corr.key[, "Form"])){
      stop(simpleError("Both answ and corr.key must contain the variable \"Form\"!"))
    } else {}
    # ok, so how many test forms are there?
    test.forms.answ <- levels(as.factor(answ[["Form"]]))
    test.forms.corr <- levels(as.factor(corr.key[, "Form"]))
    # the relevant value is what's in answ. it won't hurt if corr.key has more,
    # as long as it holds all relevant data for the forms in answ
    missing.forms <- !is.element(test.forms.answ, test.forms.corr)
    # if they are not compatible, we'll cry a little. and then stop.
    if(sum(missing.forms) > 0){
      stop(simpleError(paste("Sorry, but corr.key lacks information on test form", paste(test.forms.answ(missing.forms), collapse=", "))))
    } else {}

    # if a user accidently(?) submits only one test form, it's clearly a case for klausur()
    # we'll just hand it over and return the results
    if(length(test.forms.answ) == 1){
      warning("Only one test form was supplied. Called klausur() instead.")
      klausur.mufo.results <- klausur(answ=answ, corr=corr, marks=marks,
				      mark.labels=mark.labels, items=items, wght=wght, score=score, matn=matn,
				      na.replace=na.replace, cronbach=cronbach, item.analysis=item.analysis)
      ## calculation would end here if only one form was submitted
    }
    else {
      ## separate forms and calculate partial results
      # we'll call klausur() for each test form, and later combine the results
	  klausur.mufo.results <- new("klausuR.mult")
	  # first save the names of all test forms to the first slot
	  klausur.mufo.results@forms <- test.forms.answ
	  # create an empty object of class klausuR to store combined global results
	  klausur.mufo.global <- new("klausuR", corr=corr, marks=marks, marks.sum=marks.summary(marks))
	  for(single.test in test.forms.answ){
		answ.part <- answ[answ$Form == single.test,]
		corr.inices <- as.numeric(corr.key[corr.key[, "Form"] == single.test,][!dimnames(corr.key)[[2]] == "Form"])
		corr.part <- corr[corr.inices]
		names(corr.part) <- names(corr)
		klausur.part.results <- klausur(answ=answ.part, corr=corr.part, marks=marks,
				      mark.labels=mark.labels, items=items, wght=wght, score=score, matn=matn,
				      na.replace=na.replace, cronbach=cronbach, item.analysis=item.analysis)
		result.part <- list(Form=single.test, Results=klausur.part.results)
		klausur.mufo.results@results.part[[single.test]] <- result.part
		# append partial results to global results
		klausur.mufo.global@results 	<- rbind(klausur.mufo.global@results, klausur.part.results@results)
		klausur.mufo.global@answ 	<- rbind(klausur.mufo.global@answ, klausur.reorderItems(slot=klausur.part.results@answ, order=corr.inices))
		klausur.mufo.global@trfls 	<- rbind(klausur.mufo.global@trfls, klausur.reorderItems(slot=klausur.part.results@trfls, order=corr.inices))
		klausur.mufo.global@anon 	<- rbind(klausur.mufo.global@anon, klausur.part.results@anon)

	  }
      ## combine global results
      # missing pieces we need to calculate
      klausur.mufo.global@mean	<- summary(klausur.mufo.global@results$Points)
      klausur.mufo.global@sd	<- sd(klausur.mufo.global@results$Points)
	  ## psychometic quality of the items
	  if(isTRUE(cronbach)){
		# calling an internal function which is
		# using alpha() from package "psychometric"
		cron.alpha.list <- calc.cronbach.alpha(subset(klausur.mufo.global@trfls, select=-MatrNo))
	  } else {
	    cron.alpha.list <- list(alpha=NULL, ci=NULL, deleted=NULL)
	  }
	  if(isTRUE(item.analysis)){
		# calling another internal function which is also
		# using alpha() from package "psychometric"
		item.analyse <- calc.item.analysis(subset(klausur.mufo.global@trfls, select=-MatrNo), cron.alpha.list)
	  } else {
	    item.analyse <- NULL
	  }
      klausur.mufo.global@cronbach <- cron.alpha.list
      klausur.mufo.global@item.analysis <- item.analyse

      klausur.mufo.results@results.glob <- klausur.mufo.global
    }

    return(klausur.mufo.results)
}