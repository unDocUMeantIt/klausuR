#' Compare two data sets for use with klausuR
#'
#' The function \code{klausur.compare} will take two data sets and compare them for equality. This is useful to check for typos before
#' you calculate the results with \code{\link[klausuR:klausur]{klausur}}. If you need to type in the given answers by hand, errors 
#' easily occur, so it is advisable to input all data at least twice (perhaps by different persons) and check for differences
#' with this function, which can then be corrected by looking up the original answer in the test.
#'
#' If you don't want to compare all variables but only a subset, you can use the \code{select} option (see examples below).
#' But be careful with this, at least the variables \code{No}, \code{FirstName} and \code{Name} are needed for readable output.
#'
#' For convenience, if \code{new.set} is set to a character string, a new object is created with this name. It contains the data
#' that is identical in both sets compared, but all dubious values will be replaced by \code{NA}.
#' 
#' @title Comparison of data sets
#' @usage klausur.compare(set1, set2, select=NA, new.set=NA, rename=c())
#' @aliases klausur.compare
#' @param set1,set2 The data sets to be compared. Can be two data.frames or objects of class \code{\link[klausuR]{klausuR.answ-class}}.
#'		If the latter, their slots \code{id} and \code{items} will be compared.
#' @param select A vector with variables that should be compared, all others are omitted. At least "No", "FirstName" and "Name" are needed for the output!
#' @param new.set A character string representing a valid name for an R object
#' @param rename A named vector defining if variables in \code{set1} and \code{set2} need to be renamed into the klausuR name scheme. Accepts elements
#'		named \code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, \code{Pseudonym} and \code{Form}. The values of these elements
#'		represent the variable names of the input data.
#' @return A data.frame of the differences, if found. If not, just a message is returned.
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @keywords utilities
#' @seealso \code{\link[klausuR:klausur]{klausur}}
#' @export
#' @examples
#' \dontrun{
#' # default comparison
#' klausur.compare(antworten, antworten2)
#'
#' # compare only variables 1 to 12
#' klausur.compare(antworten, antworten2, select=c(1:12))
#'
#' # omit variables 3 to 8 and create a new set called "antworten.comp"
#' # from the results
#' klausur.compare(antworten, antworten2, select=-c(3:8),
#'   new.set="antworten.comp")
#' }

klausur.compare <- function(set1, set2, select=NA, new.set=NA, rename=c()){

	# get the names of the given sets, to better understand the outcome later
	set1.name <- deparse(substitute(set1))
	set2.name <- deparse(substitute(set2))

	# see if we have klausuR.answ class objects
	if(inherits(set1, "klausuR.answ")){
		set1 <- cbind(set1@id, subset(set1@items, select=-MatrNo))
	} else {}
	if(inherits(set2, "klausuR.answ")){
		set2 <- cbind(set2@id, subset(set2@items, select=-MatrNo))
	} else {}

	# first thing, if both sets are indeed the same, we can quit immediately
	if(identical(set1, set2)) {
		return(cat("\nThe compared objects (",set1.name," & ",set2.name,") are identical!\n", sep=""))
	} else {
		# before any values are even compared, check for equality of elements
		if(!identical(names(set1), names(set2))){
			missingVarsIn1 <- which(!names(set1) %in% names(set2))
			missingVarsIn2 <- which(!names(set2) %in% names(set1))
			missingColsIn1 <- names(set1)[missingVarsIn1]
			missingColsIn2 <- names(set2)[missingVarsIn2]
			if(length(missingVarsIn1)>0){
				set1 <- set1[,-missingVarsIn1]
				missing.message1 <- paste("Missing columns in ", set2.name, ":\n    ", paste(missingColsIn1, collapse=", "), "\n  ", sep="")
			} else {
				missing.message1 <- ""
			}
			if(length(missingVarsIn2)>0){
				set2 <- set2[,-missingVarsIn2]
				missing.message2 <- paste("Missing columns in ", set1.name, ":\n    ", paste(missingColsIn2, collapse=", "), "\n  ", sep="")
			} else {
				missing.message2 <- ""
			}
			warning(paste("\nThe objects do not include columns of the same name. Missing ones have been ignored in both:\n  ", missing.message1, missing.message2, sep=""), call.=FALSE)
		} else {}

		# do we need renaming?
		if(length(rename) > 0){
			vars.to.rename <- names(rename)
			id.names <- c("No", "Name", "FirstName", "MatrNo")
			id.possible.names <- c(id.names, "Pseudonym", "Form")
			if(any(vars.to.rename %in% names(set1))){
				double.vars <- vars.to.rename[vars.to.rename %in% names(set1)]
				warning(paste("Probably duplicate variable names found, please double check the outcome:\n ", paste(double.vars, collapse=", ")), call.=FALSE)
			} else {}
			id.invalid.names <- vars.to.rename[!vars.to.rename %in% id.possible.names]
			if(length(id.invalid.names) > 0){
				stop(simpleError(paste("Invalid variable names in 'rename':\n ",
					paste(id.invalid.names, collapse=", "))))
			} else {}
			# rename columns, if any
			for (ren.var in vars.to.rename){
				ren.from <- rename[ren.var]
				ren.to	<- ren.var
				dimnames(set1)[[2]][dimnames(set1)[[2]] == rename[ren.var]] <- ren.var
				dimnames(set2)[[2]][dimnames(set2)[[2]] == rename[ren.var]] <- ren.var
			}
		} else {}

		if(!identical(dim(set1)[1], dim(set2)[1])){
			missingVarsIn1 <- which(!set1[["MatrNo"]] %in% set2[["MatrNo"]])
			missingVarsIn2 <- which(!set2[["MatrNo"]] %in% set1[["MatrNo"]])
			missingMatrNosIn1 <- set1[missingVarsIn1,"MatrNo"]
			missingMatrNosIn2 <- set2[missingVarsIn2,"MatrNo"]
			if(length(missingVarsIn1)>0){
				set1 <- set1[-missingVarsIn1,]
				missing.message1 <- paste("Missing MatrNos in ", set2.name, ":\n    ", paste(missingMatrNosIn1, collapse=", "), "\n  ", sep="")
			} else {
				missing.message1 <- ""
			}
			if(length(missingVarsIn2)>0){
				set2 <- set2[-missingVarsIn2,]
				missing.message2 <- paste("Missing MatrNos in ", set1.name, ":\n    ", paste(missingMatrNosIn2, collapse=", "), "\n  ", sep="")
			} else {
				missing.message2 <- ""
			}
			warning(paste("\nThe objects differ in the number of oservations.  Missing ones have been ignored in both:\n  ", missing.message1, missing.message2, sep=""), call.=FALSE)
		} else {}

		## prepare sets for comparison
		# first create subsets, if necessary
		if(!is.na(select[1])){
			set1 <- subset(set1, select=select)
			set2 <- subset(set2, select=select)
			subset.varname <- paste(names(set1), collapse=", ")
			warning(paste("Only a subset was compared:\n", subset.varname, sep=""), call.=FALSE)
			# quit if the subset is identical
			if(identical(set1, set2))
		return(cat("\nThe compared objects (",set1.name," & ",set2.name,") are identical!\n\n", sep=""))
		} else{}
		# then sort the data according to LfdNr
		set1 <- set1[order(set1$No),]
		set2 <- set2[order(set2$No),]
		## done with preparations here

		## the actual comparison
		# create an array with indices of differences
		set.diff.array <- rbind(which(set1 != set2, arr.ind=TRUE), which(is.na(set1) != is.na(set2), arr.ind=TRUE))
		# total number of differences, needed for the sapply() call below
		uneq <- dim(set.diff.array)[1]
		# results are sorted by column, but we want them sorted by row
		set.diff.array <- set.diff.array[order(set.diff.array[,1]),]

		# the main act!
		differences <- t(
			sapply(1:uneq, function(x){
					if(uneq > 1) {
						idx <- set.diff.array[x,]
					} else {
						idx <- set.diff.array
					}
					varname <- names(set1)[idx[2]]
					# what values are present in both sets?
					value1 <- set1[idx[1],idx[2]]
					value2 <- set2[idx[1],idx[2]]
					# find LfdNr and full name to that case
					lfdnr <- set1[idx[1],"No"]
					name <- paste(set1[idx[1],"FirstName"],set1[idx[1],"Name"])
					result <- data.frame(No=lfdnr, Name=name, Item=varname, Set1=value1, Set2=value2, stringsAsFactors=FALSE)
					return(result)
				} # end function(x)
			) # end sapply()
		) # end t()

		# set the names of the given sets, to better understand the outcome
		colnames(differences)[4] <- set1.name
		colnames(differences)[5] <- set2.name
		rownames(differences) <- c(1:dim(differences)[1])

		# check if a new set should be created, with dubious values replaced by NA
		if(!is.na(new.set)){
			save.set.exp <- paste(new.set, " <<- set1 ; ",new.set,"[set1 != set2] <<- NA", sep="")
			eval(parse(text = save.set.exp))
		} else{}

		cat("\nThe objects differ:\n\n")
		return(differences)
	}
}