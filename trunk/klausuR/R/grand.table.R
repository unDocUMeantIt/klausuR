#' @export

grand.table <- function(NR.res=NULL, NRET.res=NULL, NRETp.res=NULL, ET.res=NULL, rescale=TRUE, file=NULL, csv2=TRUE, encoding="CP1252", ...){

	if(is.null(c(NR.res, NRET.res, NRETp.res, ET.res))){
		stop(simpleError("At least one of 'NR.res', 'NRET.res', 'NRETp.res' or 'ET.res' must be specified!"))
	}	else {}

	given.obj <- list()
	if(!is.null(NR.res)) {
		given.obj$NR <- NR.res
	} else {}
	if(!is.null(NRET.res)) {
		given.obj$NRET <- NRET.res
	} else {}
	if(!is.null(NRETp.res)) {
		given.obj$NRETp <- NRETp.res
	} else {}
	if(!is.null(ET.res)) {
		given.obj$ET <- ET.res
	} else {}
	obj.names <-  names(given.obj)

	## TODO: check if all objects have the same subjects (MatrNo)!

	# begin with id date from the first object
	if(inherits(given.obj[[1]], "klausuR")){
		new.table <- given.obj[[1]]@results[,c("No","Name","FirstName","MatrNo", "Pseudonym")]
	} else {
		stop(simpleError("All result objects must be of class klausuR!"))
	}

	# now combine all evaluations
	for (obj.idx in obj.names){
		cur.obj <- given.obj[[obj.idx]]
		if(inherits(cur.obj, "klausuR")){
			obj.name <- obj.idx
			if(isTRUE(rescale) & !identical(obj.name, "NR")){
				if(identical(obj.name, "NRETp")) {
					score <- "NRET+"
				} else {
					score <- obj.name
				}
				rescaled.obj <- nret.rescale(cur.obj, score=score)
				create.cols <- parse(text=paste("new.table <- cbind(new.table, ", obj.name,"=cur.obj@results[,c(\"Points\",\"Percent\")], ", obj.name,".rs=rescaled.obj@results[,c(\"Points\",\"Percent\")], ", obj.name,".Mark=cur.obj@results[,c(\"Mark\")])", sep=""))
				eval(create.cols)
			} else {
				create.cols <- parse(text=paste("new.table <- cbind(new.table, ", obj.name,"=cur.obj@results[,c(\"Points\",\"Percent\",\"Mark\")])", sep=""))
				eval(create.cols)
			}
		} else {
			stop(simpleError("All result objects must be of class klausuR!"))
		}
	}

	new.table <- cbind(new.table, kansw=given.obj[[1]]@answ[,-1], ktrfls=given.obj[[1]]@trfls[,-1], kpoints=given.obj[[1]]@points[,-1])
	# include misc if values present
	if(dim(given.obj[[1]]@misc)[[2]] > 1){
		new.table <- cbind(new.table, given.obj[[1]]@misc[,-1])
	} else {}

	# write to disk?
	if(!is.null(file)){
		if(isTRUE(csv2)){
			write.csv2(new.table, file=file, row.names=FALSE, fileEncoding=encoding, ...)
		} else {
			write.csv(new.table, file=file, row.names=FALSE, fileEncoding=encoding, ...)
		}
	} else {}

	return(new.table)
}
