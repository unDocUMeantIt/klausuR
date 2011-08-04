# these are internal functions that are being called by some of the methods of klausuR
# they are not exported, hence not to be called by users themselves
# and are therefore only documented by the comments in this file.

## data.check.klausur()
# this function is called by klausur() and klausur.mufo()
# for some sanity checks of the given data
data.check.klausur <- function(answ, corr, items, na.rm){

		# in case no items were specified, take variables of names "Item##" as items
		if(is.null(items)){
			items <- grep("^(Item|item)([[:digit:]]{1,3})$", names(answ))
		} else{}

		# are all needed variables present in the answers data?
		needed.names <- c("No", "Name", "FirstName", "MatrNo")
		if(any(!needed.names %in% names(answ))){
				missing.vars <- needed.names[!needed.names %in% names(answ)]
				stop(simpleError(paste("Missing variables in observation data:\n ", paste(missing.vars, collapse=", "))))
		} else {}

		# we'll check for NAs only in variables we will use, so define them here
		relevant.items <- c(grep("^Name$|^FirstName$|^MatrNo$", names(answ)), items)

		# are there missing values in answ, at least in the relevant parts?
		if(sum(is.na(answ[, relevant.items]) > 0)){
			invalid.cases.row <- sort(unique(unlist(sapply(relevant.items, function(na.var){
					return(which(is.na(answ[,na.var])))
				}))))
			invalid.cases <- unlist(sapply(invalid.cases.row, function(this.case){
					case.summary <- paste("  MatrNo ", answ[this.case, "MatrNo"], " (", paste(names(answ)[is.na(answ[this.case,])], collapse=", "), ")", sep="")
				}))
			if(isTRUE(na.rm)){
				warning(paste("NAs were present in '",deparse(substitute(answ)),"' and cases have been removed:\n",
					paste(invalid.cases, collapse="\n"), sep=""), call.=FALSE)
				answ <- answ[-invalid.cases.row,]
# 				for (na.var in relevant.items){
# 					answ <- answ[!is.na(answ[, na.var]),]
# 				}
			} else {
				warning(paste("NAs were present in '",deparse(substitute(answ)),"':\n", paste(invalid.cases, collapse=", "), sep=""), call.=FALSE)
			}
		} else{}
		if(sum(is.na(corr) > 0)){
			stop(simpleError(paste("NAs present in '",deparse(substitute(corr)),"'!\n", sep="")))
		} else{}

		# now let's check wheter all defined correct answers match the variables in answers data
		if(!setequal(names(answ[, items]),names(corr))){
			fehl.items.corr <- names(corr[!is.element(names(corr),names(answ[,items]))])
			fehl.items.answ <- names(answ[!is.element(names(answ[, items]),names(corr))])
			stop(simpleError(paste("Please check:\n  ", paste(fehl.items.corr, fehl.items.answ, collapse=", "),
			"\n  The number of items differs between observed and correct answers!", sep="")))
		}

	# return objects that have probably changed
	checked.data <- list(answ=answ, items=items)
	return(checked.data)
} ## end data.check.klausur()

## scoring.check.klausur()
# this function is called by klausur() and klausur.mufo()
# for some sanity checks of the desired scoring
scoring.check.klausur <- function(corr, marks, wght, score){

		# are there missing values?
		if(!is.null(marks)){
			if(sum(is.na(marks) > 0)){
				stop(simpleError(paste("NAs present in ",deparse(substitute(marks)),"!\n", sep="")))
			} else{}
		} else{}

		if(is.null(wght)){
			message("No weight vector (wght) given. Maximum score is determined from items and scoring method.")
		} else{
			if(length(wght) != length(corr)){
			stop(simpleError("The number of weights differs from the number if items!"))
			} else{}
		}
		if(!identical(score, "solved")){
			if(!score %in% c("partial", "liberal", "NR", "ET", "NRET", "NRET+")){
			stop(simpleError("Invalid value for score, must be either \"solved\", \"partial\", \"liberal\", \"NR\", \"ET\", \"NRET\", or \"NRET+\"!"))
			} else if(identical(score, "partial")){
			warning("Partially answered items were allowed (but only if no wrong alternative was checked).", call.=FALSE)
			} else if(identical(score, "liberal")){
			warning("Partially answered items were allowed (wrong alternatives were ignored but didn't invalidate a whole answer).", call.=FALSE)
			} else if(identical(score, "ET")){
			warning("Partially answered items were allowed (scored according to Elimination Testing).", call.=FALSE)
			} else if(score %in% c("NRET", "NRET+")){
			warning("Partially answered items were allowed (scored according to Number Right Elimination Testing).", call.=FALSE)
			} else{}
		} else{}

	return(TRUE)
} ## end scoring.check.klausur()

## gen.item.names()
# an internal function to generate item names
# takes the number of items to be generated
gen.item.names <- function(num){
    # NULL if no valid item number given
    if(num < 1){
      return(NULL)
    } else{}
    # currently, 999 items are the theoretical limit
    if(num >= 1000){
      return(NULL)
    } else{}

    items <- if(num < 10){
		  paste("Item", c(1:num), sep="")
		} else {
		  if(num < 100){
		    paste("Item", sprintf("%02d", c(1:num)), sep="")
		  } else {
		    paste("Item", sprintf("%03d", c(1:num)), sep="")
		  }
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
	warning("Cronbach's alpha calculation failed!\n", e, call.=FALSE)
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
	warning("Item analysis failed!\n", e, call.=FALSE)
	return(data.frame(NA))})
  } ## end function calc.item.analysis()

## global.results()
# glues together parts of results into an object
global.results <- function(answ, points, maxp, mark, minp=0){
    if(!is.null(answ$No)){
      results <- data.frame(  No=answ$No,
			    Name=answ$Name)
    } else {
      results <- data.frame(Name=answ$Name)
    }
    # write all desired information into the data object
    results$FirstName <- answ$FirstName
    results$MatrNo    <- answ$MatrNo
    results$Points    <- points
    results$Percent   <- round(100*((points-minp)/(maxp-minp)), digits=1)
    results$Mark      <- mark
    # if pseudonyms were given, include them for anonymous feedback
    if(!is.null(answ$Pseudonym)){
      results$Pseudonym <- answ$Pseudonym
    } else{}
  return(results)
} ## end global.results()

## anon.results()
anon.results <- function(glob.res){
    if(!is.null(glob.res$Pseudonym)){
      results <- data.frame(  Pseudonym=glob.res$Pseudonym,
				 Points=glob.res$Points,
				Percent=glob.res$Percent,
				   Mark=glob.res$Mark)
    } else {
      results <- data.frame(Anonymous="(no pseudonyms for anonymous feedback available)")
    }
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
    if(length(answ.parts) > 1){
      answ.parts <- lapply(answ.parts, function(x) paste(x, collapse=", "))
    } else {
      answ.parts <- paste(answ.parts[[1]], collapse=", ")
    }
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
  if(length(item) == 0){
    stop(simpleError("Partial results wanted, but incorrect item answers given: no item name defined!\n"))
  } else{}

  if(is.null(wght)){
    wght <- 1
  } else{}

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
			      if(length(x) > corr.length){
						return(0)
					} else {
						abs.correct <- !is.na(pmatch(x, corr.parts[item][[1]]))
						abs.false   <- sum(is.na(pmatch(x, corr.parts[item][[1]])))
						# if in strict mode, discard if more answers were checked then correct one,
						# that is, if at least one wrong answer was given, return no points at all
						if(isTRUE(strict) && abs.false > 0){
							return(0)
						} else {
							return(abs.correct)
						}
			      }
			    })
      # this corresponds to the "absolute" value
      result <- unlist(lapply(result.list, sum))
      # if we want the percentage instead:
      if(identical(mode, "percent")){
			result <- round((result * wght)/corr.length, digits=2)
		} else{}
    }
    else {
      # this corresponds to the "absolute" value
      result <- as.numeric(item.answ == corr[item])
      # percentage is irrelevant for dichotomous items,
      # but there might be a weight vector
      if(identical(mode, "percent")){
			result <- result * wght
		} else{}
    }
    return(result)
  }

  part.results <- find.partial(answ.parts, corr.parts)

  return(part.results)
} ## end partial()

## function nret.score()
# as alternative scoring functions, nret.score() implements three modes:
#  - NR: traditional number right
#      c(true.pos=1, false.pos=0, true.neg=0, false.neg=0, miss=0)
#  - ET: elimination testing (strike wrong alternatives)
#      c(true.pos=0, false.pos=0, true.neg=1, false.neg=1-num.alt, miss=0)
#  - NRET: number right elimination testing (strike wrong alternatives, mark one as true)
#      c(true.pos=1, false.pos=0, true.neg=1, false.neg=1-num.alt, miss=0)
#  - NRET+: number right elimination testing (strike wrong alternatives, mark one as true; 0 points if too many "+")
#      c(true.pos=1, false.pos=0, true.neg=1, false.neg=1-num.alt, miss=0)
#
# errors (like both alternatives marked) will be evaluated like missings, pointwise
#
# answ: given answer to one item (e.g. "-+--")
# corr: the correct pattern (like answ)
# num.alt: number of answer alternatives, counted automatically if NULL
nret.score <- function(answ, corr, score="NRET", is.true="+", is.false="-", missing="0", err="*",
	num.alt=NULL, true.false=FALSE) {

	# count answer alternatives
	if(is.null(num.alt)){
		num.alt <- nchar(corr)
	} else if(!is.numeric(num.alt)){
		stop(simpleError("Value of \"num.alt\" must be NULL or a number!"))
	}

	# in which mode will be scored?
	if(identical(score, "NR")){
		mtx <- c(true.pos=1, false.pos=0, true.neg=0, false.neg=0, miss=0, err.true=1, err.false=0)
	} else if(identical(score, "ET")){
		mtx <- c(true.pos=0, false.pos=0, true.neg=1, false.neg=as.numeric(1-num.alt), miss=0, err.true=as.numeric(1-num.alt), err.false=1)
	} else if(identical(score, "NRET")){
		mtx <- c(true.pos=1, false.pos=0, true.neg=1, false.neg=as.numeric(1-num.alt), miss=0, err.true=as.numeric(2-num.alt), err.false=1)
	} else if(identical(score, "NRET+")){
		mtx <- c(true.pos=1, false.pos=0, true.neg=1, false.neg=as.numeric(1-num.alt), miss=0)
	} else {
		stop(simpleError(paste("Unknown scoring mode:", score)))
	}

	all.results <- lapply(answ, function(curr.answ){
		# first split character vectors into atomic vectors
		answ.split <- unlist(strsplit(curr.answ, split=""))
		corr.split <- unlist(strsplit(corr, split=""))
		# check for equal length
		if(length(answ.split) != length(corr.split)){
			stop(simpleError("Given and correct answers are of unequal length!"))
		} else {}
		# check for correct input
		if(sum(!answ.split %in% c(is.true, is.false, missing, err)) > 0){
			stop(simpleError("Given answer vector includes invalid characters!"))
		} else {}
		if(sum(!corr.split %in% c(is.true, is.false, missing, err)) > 0){
			stop(simpleError("Correct answer vector includes invalid characters!"))
		} else {}

		## plausibility checks
		# too many "correct" answers?
		num.yeses <- sum(answ.split %in% is.true)
		num.trues <- sum(corr.split %in% is.true)
			# if more than one correct answer is defined, the penalty for
			# marking one "wrong" must be aligned
			if(num.trues > 1 && score %in% c("ET","NRET","NRET+")){
				mtx["false.neg"] <- (1-num.alt)/num.trues
			} else {}
		if(identical(score, "NRET+") && num.yeses > num.trues && !isTRUE(true.false)){
			result <- 0
		} else {
			# then compare answer by answer
			points <- sapply(1:length(corr.split), function(idx){
					answ.given <- as.character(answ.split[idx])
					answ.crrct <- as.character(corr.split[idx])
					if(identical(answ.given, answ.crrct)){
						if(identical(answ.given, is.true)){
							# this is a true positive
							if(isTRUE(true.false)){
								return("P")
							} else {
								return(mtx["true.pos"])
							}
						} else if(identical(answ.given, is.false)){
							# this is a true negative
							if(isTRUE(true.false)){
								return("N")
							} else {
								return(mtx["true.neg"])
							}
						} else {
							# this is impossible...
							stop(simpleError("Are you sure your answer vector is correct?!"))
						}
					} else {
						if(identical(answ.given, is.true)){
							# this is a false positive
							if(isTRUE(true.false)){
								return("p")
							} else {
								return(mtx["false.pos"])
							}
						} else if(identical(answ.given, is.false)){
							# this is a false negative
							if(isTRUE(true.false)){
								return("n")
							} else {
								return(mtx["false.neg"])
							}
						} else if(answ.given %in% c(missing, err)){
							if(isTRUE(true.false)){
								return(as.character(answ.given))
							} else {
								if(score %in% c("NR","ET","NRET")){
									# the authors didn't discuss failed answers, so this is by the book
									if(identical(answ.given, missing)){
										return(mtx["miss"])
									} else {
										# ok, it's an error. did it happen to a wrong or right alternative?
										if(identical(answ.crrct, is.true)){
											return(mtx["err.true"])
										} else {
											return(mtx["err.false"])
										}
									}
								} else {
									return(mtx["miss"])
								}
							}
						}
					}
				}
			)

			if(isTRUE(true.false)){
				# return true/false indicators, e.g. "PNNn0"
				result <- paste(points, collapse="")
			} else {
				result <- sum(points)
			}

		}

		return(result)
		})
		
	## currently, no negative points are valid for mark assignments
	# so to be sure, we'll globally add num.alt-1 points, so 0 is the minimum
	## if this gets changed, take care of the calculation of max. points as well!!!
	if(isTRUE(true.false) || identical(score, "NR")){
		all.results <- unlist(all.results)
	} else {
		all.results <- unlist(all.results) + (num.alt-1)
	}

	return(all.results)
} ## end function nret.score()

## function nret.minmax()
# compute minimum/maximum points, number of alternatives and baseline
nret.minmax <- function(corr, score="NRET", is.true="+", is.false="-", quiet=FALSE){
	# initial value for minimum score
	min.score <- 0
	# split whole answer vector
	corr.split <- unlist(strsplit(as.character(corr), split=""))
	num.trues <- sum(corr.split %in% is.true)
	num.false <- sum(corr.split %in% is.false)

	if(score %in% c("ET", "NRET", "NRET+")){
		# these need some special treatment, because there can be more points than items
		## currently, no negative points are valid for mark assignments
		# so to be sure, we'll globally add num.alt-1 points, so 0 is the minimum
		# see also the klausur.gen.marks function below, since this had to be cosidered there, too!
		#
		# get all alternatives
		num.alt.all <- nchar(corr)
		# check if they're all of equal length
		if(all(num.alt.all == num.alt.all[1])){
			num.alt <- as.numeric(num.alt.all[1])
		} else {
			num.alt <- max(num.alt.all)
			min.score <- sum(num.alt - num.alt.all)
			if(!isTRUE(quiet)){
				warning(paste("Items differ in number of answer alternatives: ", min(num.alt.all), "-", num.alt,
					"\n  Took the maximum (", num.alt, ") to determine additive constant to avoid negative points.",
					"\n  In effect, the lowest achievable score is ", min.score ," points.", sep=""), call.=FALSE)
			} else {}
		}
		# compute baseline, that is, what do you get with all missings?
		baseline <- length(num.alt.all) * (num.alt-1)
		if(!isTRUE(quiet)){
			warning(paste("The baseline (all missings) used for solved percentage is ", baseline ," points.", sep=""), call.=FALSE)
		} else {}

		if(score %in% c("NRET", "NRET+")){
			maxp <- num.trues + num.false + (length(corr) * (num.alt-1))
		}	else {
			maxp <- num.false + (length(corr) * (num.alt-1))
		}
	} else if(identical(score, "NR")){
		# in case no weights were given, count each item as one point
		maxp <- num.trues
		num.alt <- NULL
		baseline <- 0
		min.score <- 0
	} else {
		# in case this is no (NR)ET data at all
		maxp <- length(corr)
		num.alt <- NULL
		baseline <- 0
		min.score <- 0
	}

	results <- c(maxp=maxp, minp=min.score, baseline=baseline, num.alt=num.alt)
	return(results)
} ## end function nret.minmax()

## marks.summary()
# this function takes a vector with marks and returns a summarising matrix
# with the effective ranges of points and percentage for each mark defined
marks.summary <- function(marks, minp=0, add.const=0){
	# since 0 doesn't get counted, adjust add.const values below 0
	marks.levels <- levels(as.factor(marks))
	maxp <- length(marks) + add.const
	marks.matrix <- sapply(marks.levels, function(x){
		mark.min <- max(c(minp, min(which(marks == x)))) + add.const
		mark.max <- max(which(marks == x)) + add.const
		# avoid strange starting points above zero
		if((mark.min-minp) <= 1 & ((add.const >= 0 & minp >= 0) | add.const == minp)){
			mark.min.pct <- ""
		} else {
			mark.min.pct <- ceiling((mark.min-minp) / (maxp-minp) * 100)
		}
		mark.max.pct <- ceiling((mark.max-minp) / (maxp-minp) * 100)
		# if it's only one point value, don't display a range
		if(mark.min == mark.max){
			m.f.points <- sprintf("%3s",mark.min)
			m.f.pct <- sprintf("%3s",mark.min.pct)
		} else {
			m.f.points <- paste(sprintf("%3s", mark.min), " -- ", sprintf("%3s",mark.max), sep="")
			m.f.pct <- paste(sprintf("%3s", mark.min.pct), " < ", sprintf("%3s",mark.max.pct), sep="")
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
  # finally glue MatrNo back
  reordered.items <- cbind(matn.slot, part.slot.reordered)
  return(reordered.items)
} ## end klausur.reorderItems()
