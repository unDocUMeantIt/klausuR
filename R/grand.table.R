# Copyright 2009-2024 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package klausuR.
#
# klausuR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# klausuR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with klausuR.  If not, see <http://www.gnu.org/licenses/>.


#' Export results to a table
#'
#' Try this function to combine results from an evaluated test into a matrix and export it to a table, e.g. to import
#' in other software products. It can be particularily helpful for ET/NRET coded tests, if you want to compare the results of
#' different valid scoring functions.
#'
#' @note For obvious reasons At least one of \code{NR.res}, \code{NRET.res}, \code{NRETp.res} or \code{ET.res},
#'    or any combination of those, must be specified.
#'
#' @param NR.res An object of class \code{klausuR} which was evaluated according to the NR scoring policy.
#'    Usual MC tests must be given as \code{NR.res}, too.
#' @param NRET.res An object of class \code{klausuR} which was evaluated according to the NRET scoring policy.
#' @param NRETp.res An object of class \code{klausuR} which was evaluated according to the NRET+ scoring policy.
#' @param ET.res An object of class \code{klausuR} which was evaluated according to the ET scoring policy.
#' @param rescale Logical, whether ET/NRET scaled results should be rescaled by \code{\link[klausuR]{nret.rescale}}.
#' @param file Path to a file to save to, using \code{\link[rio:export]{rio::export}}. If missing, no file will be written
#'    but only the data frame returned.
#' @param ... Additional options for \code{\link[rio:export]{rio::export}}.
#' @return A data frame.
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @keywords misc
#' @importFrom utils write.csv write.csv2
#' @importFrom rio export
#' @export

grand.table <- function(
  NR.res,
  NRET.res,
  NRETp.res,
  ET.res,
  rescale=TRUE,
  file,
  ...
){

  # to avoid NOTEs from R CMD check:
  MatrNo <- NULL

  if(all(missing(NR.res), missing(NRET.res), missing(NRETp.res), missing(ET.res))){
    stop(simpleError("At least one of 'NR.res', 'NRET.res', 'NRETp.res' or 'ET.res' must be specified!"))
  }  else {}

  given.obj <- list()
  if(!missing(NR.res)) {
    given.obj$NR <- NR.res
  } else {}
  if(!missing(NRET.res)) {
    given.obj$NRET <- NRET.res
  } else {}
  if(!missing(NRETp.res)) {
    given.obj$NRETp <- NRETp.res
  } else {}
  if(!missing(ET.res)) {
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

  new.table <- cbind(
    new.table,
    kansw=subset(slot(given.obj[[1]], "answ"), select=-MatrNo),
    ktrfls=subset(slot(given.obj[[1]], "trfls"), select=-MatrNo),
    kpoints=subset(slot(given.obj[[1]], "points"), select=-MatrNo)
  )
  # include misc if values present
  if(dim(slot(given.obj[[1]], "misc"))[[2]] > 1){
    new.table <- cbind(
      new.table,
      subset(slot(given.obj[[1]], "misc"), select=-MatrNo)
    )
  } else {}

  # write to disk?
  if(!missing(file)){
    rio::export(
      x=new.table,
      file=file,
      ...
    )
  } else {}

  return(new.table)
}
