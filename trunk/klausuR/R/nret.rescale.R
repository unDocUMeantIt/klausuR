#' @export
nret.rescale <- function(res.obj, score="NRET", points=TRUE, percent=TRUE){
	if(!score %in% c("NR", "ET", "NRET", "NRET+")){
		stop(simpleError("Invalid value for score, must be either \"NR\", \"ET\", \"NRET\", or \"NRET+\"!"))
	} else {}

	nret.test.chars <- nret.minmax(corr=res.obj@corr, score=score)
	# baseline <- nret.test.chars["baseline"]
	old.sum <- res.obj@results$Points

	if(isTRUE(points)){
		answ.alt <- nret.test.chars["num.alt"]
		item.const <- answ.alt - 1
		num.items  <- dim(res.obj@answ)[[2]] - 1
		test.const <- item.const * num.items
		# correct points for items
		no.const.points <- cbind(res.obj@points$MatrNo, (res.obj@points[,2:dim(res.obj@answ)[[2]]] - item.const))
		res.obj@points <- no.const.points
		# correct sums
		no.const.sum <- old.sum - test.const
		res.obj@results$Points <- no.const.sum
		res.obj@anon$Points <- no.const.sum
		warning("The rescaled point values are probably not in sync with the points displayed for mark assignments. Keep that in mind.", call.=FALSE)
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
	return(res.obj)
}
