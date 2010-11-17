#' show method for S4 objects of class klausuR
#'
#' Prints a nice summary of the slots \code{results}, \code{anon}, \code{cronbach} and \code{item.analysis}.
#'
#' @title show method for objects of class klausuR
#' @method show klausuR
#' @param object An object of class klausuR
#' @aliases show,klausuR-method
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}
#' @keywords methods
#' @examples
#' \dontrun{
#' klausur(answ=antworten, corr=richtig, marks=notenschluessel)
#' }
#' @exportMethod show
#' @rdname show

setMethod("show", signature(object="klausuR"), function(object){
  if(length(object@item.analysis) > 1 && !is.na(object@item.analysis)){
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

  if(!is.na(object@cronbach$alpha)){
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
})
