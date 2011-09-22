#' Sort method for S4 objects of class klausuR
#'
#' Returns the given object, with global results and anonymized results sorted by the given variable.
#'
#' @title Sort method for objects of class klausuR
#' @param x An object of class \code{klausuR}
#' @param decreasing Logical, whether sorting should be sone increasing or decreasing.
#' @param sort.by An optional character string naming a variable to sort the results by. Defaults to \code{c()}, i.e. no re-ordering.
#' @aliases sort,-methods sort,klausuR-method sort.klausuR,klausuR-method
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}
#' @keywords methods
#' @examples
#' \dontrun{
#' klsr.obj <- klausur(data.obj)
#' sort(klsr.obj, sort.by="Points")
#' }
#' @exportMethod sort.klausuR
#' @rdname sort-methods
setGeneric("sort.klausuR", function(x, decreasing=FALSE, ...) standardGeneric("sort.klausuR"))

#' @rdname sort-methods
setMethod("sort.klausuR", signature(x="klausuR"), function(x, decreasing=FALSE, sort.by=c()){
	if(length(x@results) == 0){
		return(invisible(NULL))
	} else {}

	global.results <- x@results
	anon.results <- x@anon
	# sort results
	if(length(sort.by) > 0){
		if(!sort.by %in% names(global.results)){
			stop(simpleError(paste("Can't sort by '",sort.by,"', there's no such variable!", sep="")))
		} else {}
		new.order <- order(global.results[[sort.by]], decreasing=decreasing)
		global.results <- global.results[new.order,]
		anon.results <- anon.results[new.order,]
		dimnames(global.results)[[1]] <- 1:nrow(global.results)
		dimnames(anon.results)[[1]] <- 1:nrow(anon.results)
	} else {}

	x@results <- global.results
	x@anon <- anon.results
	return(x)
})
