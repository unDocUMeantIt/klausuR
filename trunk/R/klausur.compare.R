#' Compare two data sets for use with klausuR
#'
#' The function \code{klausur.compare} will take two data sets and compare them for equality. This is useful to check for typos before
#' you calculate the results with \code{\link[klausuR:klausur]{klausur}}. If you need to type in the given answers by hand, errors 
#' easily occur, so it is advisable to input all data at least twice (perhaps by different persons) and check for differences
#' with this function, which can then be corrected by looking up the original answer in the test.
#'
#' If you don't want to compare all variables but only a subset, you can use the \code{select} option (see examples below).
#' But be careful with this, at least the variables "No", "FirstName" and "Name" are needed for the output!
#'
#' For convenience, if \code{new.set} is set to a character string, a new object is created with this name. It contains the data
#' that is identical in both sets compared, but all dubious values will be replaced by NA.
#' 
#' @title Comparison of data sets
#' @usage klausur.compare(set1, set2, select=NA, new.set=NA)
#' @aliases klausur.compare
#' @param set1,set2 The data sets to be compared
#' @param select A vector with variables that should be compared, all others are omitted. At least "No", "FirstName" and "Name" are needed for the output!
#' @param new.set A character string representing a valid name for an R object
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

klausur.compare <- function(set1, set2, select=NA, new.set=NA){

  ## a function to get only the interesting parts
  find.diffs <- function(varname, set1=set1.sort, set2=set2.sort, n=1){
    if(n < 1)
      stop(simpleError("Illegal value: n < 1")) else{}

    # get the row where differences occur
    row.diff <- which(set1[[varname]] != set2[[varname]])
    if(length(row.diff) < n)
      return() else{}

    # what values are present in both sets?
    value1 <- set1[row.diff[n],varname]
    value2 <- set2[row.diff[n],varname]

    # find LfdNr and full name to that case
    lfdnr <- set1[row.diff[n],"No"]
    name <- paste(set1[row.diff[n],"FirstName"],set1[row.diff[n],"Name"])

    result <- data.frame(No=lfdnr, Name=name, Item=varname, Set1=value1, Set2=value2, stringsAsFactors=FALSE)

    # in case of more than one difference per row, we'll call this function recursively
    if(length(row.diff) == n)
      return(result)
    else {
      result <- rbind(result, find.diffs(varname,set1,set2,n+1))
      return(result)
    }
  } ## end of function find.diffs()

  # check for equality of elements
  if(sum(names(set1) != names(set2)) > 0)
    stop(simpleError("The objects do not include elements of the same name, cannot compare."))

  if(dim(set1)[1] != dim(set2)[1])
    stop(simpleError("The objects differ in the number of oservations, cannot compare."))

  # get the names of the given sets, to better understand the outcome later
  set1.name <- deparse(substitute(set1))
  set2.name <- deparse(substitute(set2))

  # first create subsets, if necessary
  if(!is.na(select[1])){
    set1 <- subset(set1, select=select)
    set2 <- subset(set2, select=select)
  }

  # replace NAs, they're always trouble, and we only care for equality, not values
  set1[is.na(set1)] <- "--"
  set2[is.na(set2)] <- "--"

  # then sort the data according to LfdNr
  set1.sort <- set1[order(set1$No),]
  set2.sort <- set2[order(set2$No),]

  # compare set1 and set2 and get TRUE/FALSE matrix
  comp.sets <- (set1.sort == set2.sort)

  # get vector with variable names
  var.names <- colnames(comp.sets)

  # see if there are any differences
  if(sum(!comp.sets) > 0){
    # first, find out which variables show differences
    vars.diff.index <- which(as.logical(rowSums(t(!comp.sets)) > 0))
    vars.diff.names <- names(set1.sort)[vars.diff.index]
    # now, find out which observations, calling the function defined above
    differences <- data.frame(No=c(), Name=c(), Item=c(), Set1=c(), Set2=c(), stringsAsFactors=FALSE)
    for(var.name in vars.diff.names){
      item.diffs <- find.diffs(var.name)
      if(is.null(dim(item.diffs)))
	differences <- rbind(differences,t(data.frame(item.diffs)))
      else{
	differences <- rbind(differences,item.diffs)
      }
    }
    # set names to the input set names
    colnames(differences)[4] <- set1.name
    colnames(differences)[5] <- set2.name
    rownames(differences) <- c(1:dim(differences)[1])

    # check if a new set should be created, with dubious values replaced by NA
    if(!is.na(new.set)){
      save.set.exp <- paste(new.set, " <<- set1.sort ; ",new.set,"[comp.sets == FALSE] <<- NA", sep="")
      eval(parse(text = save.set.exp))
    } else{}
      
    cat("The objects differ:\n\n")
    return(differences)
  }
  else
    return(cat("The compared objects (",set1.name," & ",set2.name,") are identical!\n", sep=""))
}
