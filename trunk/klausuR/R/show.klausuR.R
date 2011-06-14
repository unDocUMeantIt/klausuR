#' Show methods for S4 objects of classes klausuR and klausuR.mult
#'
#' Print a nice summary of the slots \code{results}, \code{anon}, \code{cronbach} and \code{item.analysis}.
#' If object is of class klausuR.mult, the global results for tests with several test forms are evaluated.
#'
#' @title Show methods for objects of class klausuR and klausuR.mult
#' @param object An object of class \code{klausuR} or \code{klausuR.mult}
#' @aliases show,-methods show,klausuR-method show,klausuR.mult-method
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}, \code{\link[klausuR:klausur.mufo]{klausur.mufo}}
#' @keywords methods
#' @examples
#' \dontrun{
#' klausur(answ=antworten, corr=richtig, marks=notenschluessel)
#'
#' # multiple test forms
#' klausur.mufo(answ=antworten, corr=richtig, corr.key=testformen,
#' marks=notenschluessel)
#' }
#' @exportMethod show
#' @rdname show-methods
setGeneric("show")

#' @rdname show-methods
setMethod("show", signature(object="klausuR"), function(object){
  if(length(object@results) == 0){return()}

    if(!is.na(object@item.analysis)){
      item.analysis <- data.frame(Diffc=round(object@item.analysis$Difficulty, 2),
		  DiscrPwr=round(object@item.analysis$Item.total, 2),
		  PartWhole=round(object@item.analysis$Item.Tot.woi, 2),
		  Discrim=round(object@item.analysis$Discrimination, 2))
      if(length(object@item.analysis$alphaIfDeleted) > 1 && !is.na(object@item.analysis$alphaIfDeleted)){
	item.analysis["alphaIfDeleted"] <- round(object@item.analysis$alphaIfDeleted, 2)}
      dimnames(item.analysis)[[1]] <- dimnames(object@item.analysis)[[1]]
      show.itan <- TRUE
    } else {
      show.itan <- FALSE
    }

    if(!is.null(object@cronbach$alpha)){
      cr.alpha <- paste("\t",round(object@cronbach$alpha, 2), "\n\tConfidence interval:\t",
		  round(object@cronbach$ci$LCL, 2),"-",
		  round(object@cronbach$ci$UCL, 2)," (95%)",sep="")
      if(!show.itan)
	cr.deleted <- round(object@cronbach$deleted, 2)
      show.alpha <- TRUE
    } else {
      show.alpha <- FALSE
    }

    cat("\nKlausuR results:")
    cat("\n\nMarks defined:\n")
    print(object@marks.sum)
    cat("\n\nGlobal results:\n")
    print(object@results)
    cat("\n\nAnonymised results:\n")
    print(object@anon)
    cat("\n\nDescriptive statistics:\n")
    print(object@mean)
    cat("\n  Sd:",round(object@sd, 2))
    if(show.alpha){
      cat("\n\nInternal consistency:\n")
      cat("\tCronbach's alpha:", cr.alpha)
      if(show.alpha && !show.itan){
	cat("\n\n")
	print(cr.deleted)
      }
    }
    if(show.itan){
      cat("\n\nItem analysis:\n")
      print(item.analysis)
    }
	cat("\n\n")
})

#' @rdname show-methods
setMethod("show", signature(object="klausuR.mult"), function(object){
  if(is.null(object@forms)){return()}

  show(object@results.glob)
  cat("\n\nThese are combined results for the test forms:\n")
  print(object@forms)
  cat("\n\nYou can get the partial results for each test form by:\n")
  cat("<object.name>@results.part\n\n")
})
