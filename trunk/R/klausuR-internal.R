# these are internal functions that are being called by some of the methods of klausuR
# they are not exported, hence not to be called by users themselves
# and are therefore only documented by the comments in this file.

## data.check.klausur()
# this function is called by klausur() and klausur.mufo()
# for some sanity checks of the given data
data.check.klausur <- function(answ, corr, marks, items, wght, score, na.replace){
		  # are there missing values in answ?
		  if(sum(is.na(answ) > 0)){
		    if(!is.null(na.replace)){
		      warning(paste("NAs were present in ",deparse(substitute(answ))," and replaced by \"",na.replace,"\"!\n", sep=""))
		      answ[is.na(answ)] <- na.replace
		    }
		    else
		      stop(simpleError(paste("NAs present in ",deparse(substitute(answ)),"!\n", sep="")))
		  } else{}
		  if(sum(is.na(corr) > 0)){
		      stop(simpleError(paste("NAs present in ",deparse(substitute(corr)),"!\n", sep="")))
		  } else{}
		  if(sum(is.na(marks) > 0)){
		      stop(simpleError(paste("NAs present in ",deparse(substitute(marks)),"!\n", sep="")))
		  } else{}

		  # are all needed variables present in the answers data?
		  if(is.null(answ$Name) || is.null(answ$FirstName) || is.null(answ$MatrNo)){
		    stop(simpleError("The observation data is not complete (Name, FirstName, MatrNo)!"))
		  } else{}

		  # in case no items were specified, take variables of names "Item##" as items
		  if(is.null(items)){
		    items <- grep("Item([[:digit:]]{1,3})", names(answ))
		  } else{}
		  # now let's check wheter all defined correct answers match the variables in answers data
		  if(!setequal(names(answ[, items]),names(corr))){
		    fehl.items.corr <- names(corr[!is.element(names(corr),names(answ[,items]))])
		    fehl.items.answ <- names(answ[!is.element(names(answ[, items]),names(corr))])
		    stop(simpleError(paste("Please check:", fehl.items.corr, fehl.items.answ, "\n\n The number of items differs between observed and correct answers!")))
		  }
		  if(is.null(wght)){
		    warning("No weight vector (wght) given.\n  Number of items is used as maximum score.")
		  } else{
		    if(length(wght) != length(corr))
		      stop(simpleError("The number of weights differs from the number if items!"))
		  }
		  if(!identical(score, "solved")){
		    if(!identical(score, "partial") && !identical(score, "liberal"))
		      stop(simpleError("Invalid value for score, must be either \"solved\", \"partial\" or \"liberal\"!"))
		    else if(identical(score, "partial"))
		      warning("Partially answered items were allowed (but only if no wrong alternative was checked).")
		    else if(identical(score, "liberal"))
		      warning("Partially answered items were allowed (wrong alternatives were ignored but didn't invalidate a whole answer).")
		  } else{}

    # return objects that have probably changed
    checked.data <- list(answ=answ, items=items)
    return(checked.data)
} ## end data.check.klausur()

## gen.item.names()
# an internal function to generate item names
# takes the number of items to be generated
gen.item.names <- function(num){
    # NULL if no valid item number given
    if(num < 1)
      return(NULL) else{}
    # currently, 999 items are the theoretical limit
    if(num >= 1000)
      return(NULL) else{}

    items <-	if(num < 10)
		  paste("Item", c(1:num), sep="")
		else {
		  if(num < 100)
		    paste("Item", sprintf("%02d", c(1:num)), sep="")
		  else
		    paste("Item", sprintf("%03d", c(1:num)), sep="")
		  }
    return(items)
  } ## end gen.item.names()

## calc.cronbach.alpha()
# calculates cronbachs alpha, needs a matrix with dichotomous data
# this function uses the package "psychometric"!
calc.cronbach.alpha <- function(dichot.matrix){
	try.cron.alpha <- function(){
	  cr.alpha <- alpha(dichot.matrix)
	  cr.alpha.ci <- alpha.CI(cr.alpha, k=dim(dichot.matrix)[2], N=dim(dichot.matrix)[1], level=0.95)
	  cr.deleted <- as.matrix(sapply(1:dim(dichot.matrix)[2], function(x){alpha(dichot.matrix[-x])}))
	rownames(cr.deleted) <- names(dichot.matrix)
	colnames(cr.deleted) <- "alphaIfDeleted"
	  return(list(alpha=cr.alpha, ci=cr.alpha.ci, deleted=cr.deleted))
	}
	cron.alpha.list <- tryCatch(try.cron.alpha(), error=function(e){
	warning("Cronbach's alpha calculation failed!\n", e)
	return(list(alpha=NA, ci=NA, deleted=NA))})
  } ## end function calc.cronbach.alpha()

## calc.item.analysis()
# performes basic item analysis, like discrimatory power etc.
# this function uses the package "psychometric"!
calc.item.analysis <- function(dichot.matrix, cron.alpha.list){
	try.item.analysis <- function(){
	  item.anal <- cbind(item.exam(dichot.matrix, discrim=TRUE), alphaIfDeleted=cron.alpha.list$deleted)
	  return(item.anal)
	}
	item.analyse <- tryCatch(try.item.analysis(), error=function(e){
	warning("Item analysis failed!\n", e)
	return(data.frame(NA))})
  } ## end function calc.item.analysis()

## global.results()
# glues together parts of results into an object
global.results <- function(answ, points, maxp, mark){
    if(!is.null(answ$No)){
      results <- data.frame(  No=answ$No,
			    Name=answ$Name)
    }
    else
      results <- data.frame(Name=answ$Name)
    # write all desired information into the data object
    results$FirstName <- answ$FirstName
    results$MatrNo    <- answ$MatrNo
    results$Points    <- points
    results$Percent   <- round(100*(points/maxp), digits=1)
    results$Mark      <- mark
    # if pseudonyms were given, include them for anonymous feedback
    if(!is.null(answ$Pseudonym))
      results$Pseudonym <- answ$Pseudonym
    else{}
  return(results)
} ## end global.results()

## anon.results()
anon.results <- function(glob.res){
    if(!is.null(glob.res$Pseudonym)){
      results <- data.frame(  Pseudonym=glob.res$Pseudonym,
				 Points=glob.res$Points,
				Percent=glob.res$Percent,
				   Mark=glob.res$Mark)
    }
    else
      results <- data.frame(Anonymous="(no pseudonyms for anonymous feedback available)")
  return(results)
} ## end anon.results()

## answ.alternatives()
# splits items with multiple answer alternatives into its parts
# the result can be a list with every alternative,
# or, if latex=TRUE, one object with all alternatives separated by commas,
# which is used for nicer looking output in klausur.report()
answ.alternatives <- function(answ, latex=FALSE){
  # divide all answers into its parts
  answ.parts <- strsplit(as.character(answ), "")
  names(answ.parts) <- names(answ)
  if(isTRUE(latex)){
    if(length(answ.parts) > 1)
      answ.parts <- lapply(answ.parts, function(x) paste(x, collapse=", "))
    else
      answ.parts <- paste(answ.parts[[1]], collapse=", ")
  } else{}

  return(answ.parts)
} ## end answ.alternatives()

## partial()
# this function computes results of one item including partially correct answers
# if multiple correct answer alternatives are possible.
# item.answ must be a vector of given answers to one item with the item name given
#
# mode can be
# - "absolute" (number of correct alternatives, ignores weights -- default)
# - "percent"  (percent of correct alternatives, can be combined with wght to weight the result)
#
# if strict=TRUE, only answers are counted if *no* wrong alternative was checked at all
partial <- function(item.answ, corr, wght=NULL, mode="absolute", strict=TRUE){
  # check for partially correct answers
  # firstly, extract the item name from the answ vector
  item <- names(item.answ)
  if(length(item) == 0)
    stop("Partial results wanted, but incorrect item answers given: no item name defined!\n")
  else{}

  if(is.null(wght))
    wght <- 1
  else{}

  # divide all correct answers into their parts
  corr.parts <- answ.alternatives(corr)
  answ.parts <- lapply(item.answ, answ.alternatives)

  find.partial <- function(answers, corr.parts){
    # how many correct answers are there?
    corr.length <- length(corr.parts[item][[1]])
    if(corr.length > 1){
      result.list <- lapply(answers[[1]],
			    function(x){
			   # count only if no more answers were checked than correct answers available
			      if(length(x) > corr.length)
				return(0)
			      else
				abs.correct <- !is.na(pmatch(x, corr.parts[item][[1]]))
				abs.false   <- sum(is.na(pmatch(x, corr.parts[item][[1]])))
			   # if in strict mode, discard if more answers were checked then correct one,
			   # that is, if at least one wrong answer was given, return no points at all
			      if(strict && abs.false > 0)
				return(0)
			      else
				return(abs.correct)
			    })
      # this corresponds to the "absolute" value
      result <- unlist(lapply(result.list, sum))
      # if we want the percentage instead:
      if(identical(mode, "percent"))
	result <- round((result * wght)/corr.length, digits=2)
      else{}
    }
    else {
      # this corresponds to the "absolute" value
      result <- as.numeric(item.answ == corr[item])
      # percentage is irrelevant for dichotomous items,
      # but there might be a weight vector
      if(identical(mode, "percent"))
	result <- result * wght
      else{}
    }
    return(result)
  }

  part.results <- find.partial(answ.parts, corr.parts)

  return(part.results)
} ## end partial()

## marks.summary()
# this function takes a vector with marks and returns a summarising matrix
# with the effective ranges of poibts and percentage for each mark defined
marks.summary <- function(marks){
  marks.levels <- levels(as.factor(marks))
  maxp <- length(marks)
  marks.matrix <- sapply(marks.levels, function(x){
    mark.min <- min(which(marks == x))
    mark.max <- max(which(marks == x))
    # avoid strange starting points above zero
    if(mark.min <= 1){
      mark.min.pct <- ""
    } else {
      mark.min.pct <- ceiling(mark.min / maxp * 100)
    }
    mark.max.pct <- ceiling(mark.max / maxp * 100)
    # if it's only one point value, don't display a range
    if(mark.min == mark.max){
      m.f.points <- mark.min
      m.f.pct <- mark.min.pct
    } else {
      m.f.points <- paste(mark.min, "-", mark.max, sep="")
      m.f.pct <- paste(mark.min.pct, " < ", mark.max.pct, sep="")
    }

    mark.frame <- c(
      Points=m.f.points,
      Percent=m.f.pct
      )
    return(mark.frame)
    }
  )
  return(t(marks.matrix))
} ## end marks.summary()

## klausur.reorderItems()
# put items in correct order (multiple test forms)
klausur.reorderItems <- function(slot, order){
  part.slot <- subset(slot, select=-MatrNo)
  slot.names <- names(part.slot)
  matn.slot <- subset(slot, select=MatrNo)
  # now let's reorder the stuff
  part.slot.reordered <- part.slot[,order]
  names(part.slot.reordered) <- slot.names
  # finally glue MatNo back
  reordered.items <- cbind(matn.slot, part.slot.reordered)
  return(reordered.items)
} ## end klausur.reorderItems()
