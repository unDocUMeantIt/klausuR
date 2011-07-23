#' A function to create data objects with given and correct answers to a test.
#'
#' @param answ A \code{\link{data.frame}} which has to include at least these variables:
#'	\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, as well as \code{Pseudonym} (optional)
#'	and variables for the answered items (according to the scheme \code{Item###},
#'	where ### is a number with leading zeros, if needed).
#' @param corr A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).
#' @param items Indices of a subset of variables in \code{answ} to be taken as items.
#' @param marks A vector assigning marks to points achieved (see details). Leave \code{NULL} if not available.
#' @param wght A vector with weights for each item (named also according to \code{Item###}). Leave \code{NULL} if not available.
#' @param corr.key If test has several test forms: A data.frame or matrix indicating the positions of all items (columns) in all
#'		forms (rows). Must have a column called \code{Form} (like \code{answ}), and the item columns must follow the explained name
#'		scheme \code{Item###}. \code{NULL} if not needed.
#' param rename A named vector defining if variables in \code{answ} need to be renamed into the klausuR name scheme. Accepts elements
#'		named \code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, \code{Pseudonym} or \code{Form}. The values of these elements
#'		represent the variable names of the input data.
#' @param na.rm Logical, whether cases with NAs should be ignored in \code{answ}. Defaults to TRUE.
#' @return An object of class \code{\link[klausuR]{klausuR.answ-class}}.
#' @export

klausur.data <- function(answ, corr, items=NULL, marks=NULL, wght=NULL, corr.key=NULL, rename=c(), na.rm=TRUE){

	# in case no items were specified, take variables of names "Item##" as items
	if(is.null(items)){
		items <- grep("^(Item|item)([[:digit:]]{1,3})$", names(answ))
	} else{}

	vars.to.rename <- names(rename)
	id.names <- c("No", "Name", "FirstName", "MatrNo")
	id.possible.names <- c(id.names, "Pseudonym", "Form")

	# check for name collisions -- do we end up with doubled varnames?
	if(length(vars.to.rename) > 0){
		if(any(vars.to.rename %in% names(answ))){
			double.vars <- vars.to.rename[vars.to.rename %in% names(answ)]
			warning(paste("Probably duplicate variable names found, please double check the outcome:\n ", paste(double.vars, collapse=", ")), call.=FALSE)
		} else {}
	} else {}
	id.invalid.names <- vars.to.rename[!vars.to.rename %in% id.possible.names]
	if(length(id.invalid.names) > 0){
		stop(simpleError(paste("Invalid variable names in 'rename':\n ",
			paste(id.invalid.names, collapse=", "))))
	} else {}

	# exclude certain cases?

	# rename columns, if any
	for (ren.var in vars.to.rename){
		ren.from <- rename[ren.var]
		ren.to	<- ren.var
		dimnames(answ)[[2]][dimnames(answ)[[2]] == rename[ren.var]] <- ren.var
	}

	sane.data <- data.check.klausur(answ=answ, corr=corr, items=items, na.rm=na.rm)
	stopifnot(scoring.check.klausur(corr=corr, marks=marks, wght=wght, score="solved"))
	answ <- sane.data$answ
	items <- sane.data$items

	# convert probable factors to character, and trimming values
	found.vars <- names(answ)[names(answ) %in% c("Name", "FirstName", "MatrNo", "Pseudonym")]
	for (char.var in found.vars){
		answ[[char.var]] <- gsub("(^[[:space:]]+)|([[:space:]]+$)", "", as.character(answ[[char.var]]))
	}
	
	# sort data by MatrNo
	answ <- answ[order(answ$MatrNo),]

	# prepare columns for resulting data.frames
	if("Pseudonym" %in% names(answ)){
		id.pseudonym <- answ[["Pseudonym"]]
	} else {
		id.pseudonym <- NA
	}
	if("Form" %in% names(answ)){
		id.form <- factor(answ[["Form"]])
	} else {
		id.form <- NA
	}
	# collect the rest for the 'misc' slot
	unused.stuff <- answ[, !names(answ) %in% c(id.possible.names, names(answ[, items]))]
	misc.data <- data.frame(MatrNo=answ[["MatrNo"]], unused.stuff)

	# create resulting object
	results <- new("klausuR.answ",
		corr=list(corr=corr, corr.key=corr.key),
		id=data.frame(
			No=answ[["No"]],
			Name=answ[["Name"]],
			FirstName=answ[["FirstName"]],
			MatrNo=answ[["MatrNo"]],
			Pseudonym=id.pseudonym,
			Form=id.form, stringsAsFactors=FALSE),
		items=data.frame(MatrNo=answ[["MatrNo"]], answ[, items], stringsAsFactors=FALSE),
		score=list(marks=marks, wght=wght),
		misc=misc.data)

	return(results)
}
