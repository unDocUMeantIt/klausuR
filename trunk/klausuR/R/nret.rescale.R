#' @export
nret.rescale <- function(res.obj, score="NRET", points=TRUE, percent=TRUE, marks=TRUE){
	if(!score %in% c("NR", "ET", "NRET", "NRET+")){
		stop(simpleError("Invalid value for score, must be either \"NR\", \"ET\", \"NRET\", or \"NRET+\"!"))
	} else {}

	nret.test.chars <- nret.minmax(corr=res.obj@corr, score=score, quiet=TRUE)
	# baseline <- nret.test.chars["baseline"]
	old.sum <- res.obj@results$Points
	answ.alt <- nret.test.chars["num.alt"]
	item.const <- answ.alt - 1
	num.items  <- dim(res.obj@answ)[[2]] - 1
	test.const <- item.const * num.items

	if(isTRUE(points)){
		# correct points for items
		no.const.points <- cbind(res.obj@points$MatrNo, (res.obj@points[,2:dim(res.obj@answ)[[2]]] - item.const))
		res.obj@points <- no.const.points
		# correct sums
		no.const.sum <- old.sum - test.const
		res.obj@results$Points <- no.const.sum
		res.obj@anon$Points <- no.const.sum
		if(!isTRUE(marks)){
			warning("The rescaled point values are probably not in sync with the points displayed for mark assignments. Keep that in mind.", call.=FALSE)
		} else {}
	} else {}
	# change percentage?
	if(isTRUE(percent)){
		maxp <- nret.test.chars["maxp"]
		minp <- nret.test.chars["minp"]
		new.percentage <- round(100*((old.sum-minp)/(maxp-minp)), digits=1)
		res.obj@results$Percent <- new.percentage
		res.obj@anon$Percent <- new.percentage
	} else {}
	# finished
	if(isTRUE(marks)){
		new.marks.sum <- marks.summary(res.obj@marks, minp=-test.const, add.const=-test.const)
		res.obj@marks.sum <- new.marks.sum
		# overwrite the old vector, it's not valid here
		res.obj@marks <- c("Removed")
	} else {}
	return(res.obj)
}
