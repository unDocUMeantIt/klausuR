#' Class klausuR
#'
#' This class is used for objects that are returned by \code{\link[klausuR:klausur]{klausur}}.
#'
#' @title S4 class klausuR
#' @slot results A data.frame with global results
#' @slot answ A data.frame with all given answers
#' @slot corr A vector with the correct answers
#' @slot wght A vector with the weights of items
#' @slot points A data.frame with resulting points given for the answers
#' @slot marks A vector with assignments of marks to achieved score
#' @slot marks.sum A more convenient matrix with summary information on the defined marks
#' @slot trfls A data.frame of TRUE/FALSE values, whether a subject was able to solve an item or not
#' @slot anon A data.frame for anonymous feedback
#' @slot mean A table with mean, median and quartiles of the test results
#' @slot sd Standard deviation of the test results
#' @slot cronbach Internal consistency, a list of three elements "alpha", "ci" (confidence interval 95\%) and "deleted" (alpha if item was removed)
#' @slot item.analysis A data.frame with information on difficulty, discriminant power and discriminant factor of all items.
#' @name klausuR,-class
#' @import methods
#' @keywords classes
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @exportClass klausuR
#' @rdname klausuR-class

setClass("klausuR",
    representation=representation(results="data.frame",
	answ="data.frame",
	corr="vector",
	wght="vector",
	points="data.frame",
	marks="vector",
	marks.sum="matrix",
	trfls="data.frame",
	anon="data.frame",
	mean="table",
	sd="numeric",
	cronbach="list",
	item.analysis="data.frame"),
    prototype(results=data.frame(),
	answ=data.frame(),
	corr=NULL,
	wght=NULL,
	points=data.frame(),
	marks=NULL,
	marks.sum=NULL,
	trfls=data.frame(),
	anon=data.frame(),
	mean=table(NULL),
	sd=numeric(),
	cronbach=list(alpha=NULL, ci=NULL, deleted=NULL),
	item.analysis=data.frame())
)

#setValidity("klausuR", function(object){
#  
#})
