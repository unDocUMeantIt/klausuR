# Copyright 2009-2015 Meik Michalke <meik.michalke@hhu.de>
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


#' Generate individual reports on multipe choice test results
#'
#' \code{klausur.report} takes (at least) an object of class klausuR (or klausuR.mult) and a matriculation number to generate personal test results
#' in LaTeX and/or PDF format.
#'
#' The report contains, next to the individual results, a table with all given and correct  answers (using \code{\link[xtable]{xtable}}),
#' as well as nice histograms showing the distribution of the test results (points and/or marks are supportet). If the matriculation numer
#' is set to "all", reports for all subjects are produced. Setting it to "anon" will get you a printable version of the anonymized results.
#' 
#' By default output is sent to standard out. To save them to disk in LaTeX format a "save" parameter is provided. Alternatively, the reports
#' can be converted to PDF format as well. \code{klausur.report} is calling \code{\link[tools]{texi2dvi}} from the \code{tools} package for that.
#'
#' If the object is of class klausuR.mult, only the global results for tests with several test forms are evaluated. In case you'd rather like
#' reports on each test form, call \code{klausur.report} with the single slots from that object accordingly.
#'
#' @param klsr An object of class klausuR or klausuR.mult. To create reports from more than one object with the same configuration, you can
#'    also give them in one list here, which will cause the function to call itself recursively.
#' @param matn Matriculation number, "all" (produces individuall documents for all subjects), "anon" (produces anonymous feedback)
#'  or "glob" (produces a global results document).
#' @param save Logical: If TRUE, files are saved to disk (scheme: "\code{path}/\code{matn}.tex").
#' @param pdf Logical: If TRUE, LaTeX reports will be converted to PDF automatically, using \code{\link[tools]{texi2dvi}}.
#'  If \code{save} is FALSE, a temporary directory is used, that is only the PDF files will be saved.
#' @param path Path for \code{save} and \code{hist} files.
#' @param file.name File name scheme for the reports, either "matn" (matriculation number) or "name" (name and firstname).
#' @param hist A list with the logical elements \code{points} and \code{marks}: If TRUE, the reports will include histograms
#'  of the distribution of points and/or marks. The needed PDF files will be created by \code{\link[klausuR:plot]{plot}} and saved as well.
#'  (see \code{path}, \code{hist.points} and \code{hist.marks}).
#' @param hist.merge If you need/want to combine results from several \code{klausuR} class objects for the histograms, provide them all in a list here.
#' @param hist.points File name for the histogram of points.
#' @param hist.marks File name for the histogram of marks.
#' @param descr Details on the test: List with the elements \code{title} (title of the test), \code{name} (your name) and \code{date}.
#' @param marks.info A list with the logical elements \code{points} and \code{percent}: If TRUE, the reports will include a table showing
#'  how marks were assigned to points achieved and/or percent solved, respectively.
#' @param lang Set to "de" for reports in German, English is the default.
#' @param alt.candy If TRUE, a comma will be inserted for items with multiple alternatives ("235" becomes "2, 3, 5" in the printout)
#' @param anon.glob.file If \code{matn="anon"} or \code{matn="glob"}, you can specify a filename for this particular report.
#' @param decreasing Logical, whether sorting of output should be done increasing or decreasing (only relevant for \code{matn="anon"} or
#'  \code{matn="glob"}).
#' @param sort.by Character string naming a variable to sort the results by. Defaults to \code{"Marks"} (only relevant for \code{matn="anon"} or
#'  \code{matn="glob"}).
#' @param NRET.legend Logical, If ET/NRET data is reported, you can demand a legend in the table caption by setting this to true.
#' @param table.size Character string to shrink the tables, must be one of \code{"auto"}, \code{"normalsize"}, \code{"small"},
#'    \code{"footnotesize"}, \code{"scriptsize"} or \code{"tiny"}. The default \code{table.size="auto"} tries to decide between
#'    \code{"normalsize"} and \code{"footnotesize"} to avoid pages with only one or two rows. If that fails, try to manually set the size.
#' @param merge Logical, if \code{TRUE} no individual PDFs will be saved, but one large file with all reports. Uses the "pdfpages" package,
#'    and only useful if \code{pdf=TRUE} as well.
#' @param quiet Logical, if \code{TRUE} no feedback messages on the current status are given.
#' @param fancyhdr Logical, if \code{TRUE} additional information is printed in the header and footer of the LaTeX/PDF files.
#' @aliases klausur.report
#' @keywords IO file
#' @return One or several LaTeX and/or PDF documents. If defined two histograms will be plotted.
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}, \code{\link[xtable]{xtable}}, \code{\link[tools]{texi2dvi}}
#' @import xtable graphics tools
#' @export
#' @examples
#' data(antworten)
#' 
#' # vector with correct answers:
#' richtig <- c(Item01=3, Item02=2, Item03=2, Item04=2, Item05=4,
#'  Item06=3, Item07=4, Item08=1, Item09=2, Item10=2, Item11=4,
#'  Item12=4, Item13=2, Item14=3, Item15=2, Item16=3, Item17=4,
#'  Item18=4, Item19=3, Item20=5, Item21=3, Item22=3, Item23=1,
#'  Item24=3, Item25=1, Item26=3, Item27=5, Item28=3, Item29=4,
#'  Item30=4, Item31=13, Item32=234)
#' 
#' # vector with assignement of marks:
#' notenschluessel <- c()
#' # scheme of assignments: marks[points_from:to] <- mark
#' notenschluessel[0:12]  <- 5.0
#' notenschluessel[13:15] <- 4.0
#' notenschluessel[16:18] <- 3.7
#' notenschluessel[19:20] <- 3.3
#' notenschluessel[21]    <- 3.0
#' notenschluessel[22]    <- 2.7
#' notenschluessel[23]    <- 2.3
#' notenschluessel[24]    <- 2.0
#' notenschluessel[25:26] <- 1.7
#' notenschluessel[27:29] <- 1.3
#' notenschluessel[30:32] <- 1.0
#' 
#' data.obj <- klausur.data(answ=antworten, corr=richtig, marks=notenschluessel)
#' klsr.obj <- klausur(data.obj)
#'
#' \dontrun{
#' klausur.report(klsr=klsr.obj, matn="all", descr=list(title="Klausur Tatort",
#'   name="Dr. T. Aeter", date="24.09.2010"))
#' }

klausur.report <- function(klsr, matn, save=FALSE, pdf=FALSE, path=NULL, file.name="matn",
          hist=list(points=FALSE, marks=FALSE), hist.merge=list(), hist.points="hist_points.pdf", hist.marks="hist_marks.pdf",
          descr=list(title=NULL, name=NULL, date=NULL), marks.info=list(points=FALSE, percent=FALSE),
          lang="en", alt.candy=TRUE, anon.glob.file="anon.tex", decreasing=TRUE, sort.by="Points", NRET.legend=FALSE, table.size="auto",
          merge=FALSE, quiet=FALSE, fancyhdr=TRUE){
  # to avoid NOTEs from R CMD check:
  marks.information <- NULL

  # before we start let's look at klsr
  # if klsr is a list, iterate through it recusively
  if(is.list(klsr)){
    for(this.klsr in klsr){
      klausur.report(klsr=this.klsr, matn=matn, save=save, pdf=pdf, path=path, file.name=file.name,
        hist=hist, hist.points=hist.points, hist.marks=hist.marks,
        descr=descr, marks.info=marks.info, lang=lang, alt.candy=alt.candy, anon.glob.file=anon.glob.file,
        decreasing=decreasing, sort.by=sort.by, NRET.legend=NRET.legend, table.size=table.size, quiet=quiet, fancyhdr=fancyhdr)
    }
    return("done")
  } else {}
  # check if output needs to be sorted
  if(identical(matn, "anon") | identical(matn, "glob")){
    klsr <- sort(klsr, decreasing=decreasing, sort.by=sort.by)
  } else {}
  # if it's of class "klausuR.mult", extract global results and drop the rest
  if(inherits(klsr, "klausuR.mult")){
    klsr <- klsr@results.glob
  } else{
    # check whether klsr is an object of class "klausuR" instead
    if(!inherits(klsr, "klausuR")){
      stop(simpleError("The given object is not of class \"klausuR\"!"))
    } else {}
  }

  if(!is.numeric(matn) && !identical(matn, "all") && !identical(matn, "anon") && !identical(matn, "glob")){
    stop(simpleError("Value assigned to matn must be numeric, \"all\", \"anon\" or \"glob\"!"))
  } else {}

  if((isTRUE(save) || isTRUE(pdf) || isTRUE(hist$points) || isTRUE(hist$marks)) && is.null(path)){
    stop(simpleError("Files have to be saved, but path is empty!"))
  } else {}

  ## path handling
  # check if path exists
  if(!isTRUE(file.info(path)$isdir)) {
    stop(simpleError(paste(path,"is not a valid path!")))
  } else {}
  # PDF creation will be done in an temporal directory if "save" is FALSE
  # we'll make "path" "path.orig" and override it with that tempdir internally
  if(!isTRUE(save) && isTRUE(pdf)){
    path.orig <- path
    path <- tempfile("klausuR")
    if(!dir.create(path, recursive=TRUE)){
      stop(simpleError("Couldn't create temporary directory! Try with save=TRUE"))
    } else {}
    # if the function is done, remove the tempdir
    on.exit(
      if(!identical(path, path.orig)){
        unlink(path, recursive=TRUE)
      } else {}
    )
  } else {}

  # check value for table.size
  if(!table.size %in% c("auto", "normalsize", "small", "footnotesize", "scriptsize", "tiny")){
    warning(paste("Invalid value for 'tabe.size':\n  ", table.size,"\n  Reverted to \"normal\"."), call.=FALSE)
    table.size <- "auto"
  } else {}

  # define the text of the LaTeX document...
  if(identical(lang, "de")){
    text <- list(Auswertung="Einzelauswertung",
        DozentIn="Dozent",
        MatrikelNr="Matrikel-Nr.",
        Datum="Datum der Klausur",
        Antwort="Antwort",
        Korrekt="L\u00f6sung",
        Punkte="Punkte",
        Ergebnisse="Ergebnisse",
        Erreicht="Erreichte Punktzahl",
        Prozent=" der erreichbaren Punkte",
        Note="Daraus ergibt sich die \\textbf{Note",
        VertErgs="Verteilung der Klausurergebnisse",
        Pseudonym="Pseudonym",
        AProzent="Prozent",
        ANote="Note",
        LfdNr="Nr.",
        GMatNr="MatrNr.",
        Name="Name",
        Vorname="Vorname",
        Notenschluessel="Notenschl\"ussel",
         NRET.expl=". \\emph{Erl\"auterung:} >>+<< -- richtig; >>-<< -- falsch; >>0<< -- keine Angabe; >>*<< -- fehlerhafte Angabe."
    )
    # ... and that of the plots
    hist.text <- list(P.xlab="Punkte",P.ylab="H\u00e4ufigkeit",P.main="Verteilung nach Punkten",
        N.xlab="Note",N.ylab="H\u00e4ufigkeit",N.main="Verteilung nach Noten"
    )
  } ## end of german l10n
  else {
    text <- list(Auswertung="Individual Report",
        DozentIn="Docent",
        MatrikelNr="Matriculation No.",
        Datum="Date of test",
        Antwort="Answer",
        Korrekt="Solution",
        Punkte="Points",
        Ergebnisse="Results",
        Erreicht="Achieved score",
        Prozent=" of achievable points",
        Note="This score gives \\textbf{mark",
        VertErgs="Distribution of test results",
        Pseudonym="Pseudonym",
        AProzent="Percent",
        ANote="Mark",
        LfdNr="No.",
        GMatNr="MatrNo.",
        Name="Name",
        Vorname="First name",
        Notenschluessel="Marks defined",
        NRET.expl=". \\emph{Explaination:} >>+<< -- right; >>-<< -- wrong; >>0<< -- not answered; >>*<< -- errenous answer."
    )
    # ... and that of the plots
    hist.text <- list(P.xlab="Points",P.ylab="Frequency",P.main="Distribution by points",
        N.xlab="Marks",N.ylab="Frequency",N.main="Distribution by marks"
    )
  }

  klsr.to.plot <- klsr
  if(is.list(hist.merge) & length(hist.merge) > 0){
    klsr.to.plot@results <- plot.merger(hist.merge)
  } else {}

  if(hist$points | hist$marks) {
    if(hist$points) {
      pdf(file=file.path(path, hist.points),
      width=10, height=10,
      pointsize=22, bg="white")
      plot(klsr.to.plot, xlab=hist.text$P.xlab, ylab=hist.text$P.ylab, main=hist.text$P.main)
      dev.off()
    } else {}

    if(hist$marks) {
      pdf(file=file.path(path, hist.marks),
      width=10, height=10,
      pointsize=22, bg="white")
      plot(klsr.to.plot, marks=TRUE, xlab=hist.text$N.xlab, ylab=hist.text$N.ylab, main=hist.text$N.main)
      dev.off()
    } else {}
  } else {}

  ## let's grab some info out of the klausuR-object for code readability
  results <- klsr@results
  res.points <- klsr@points
  truefalse <- klsr@trfls
  wght <- klsr@wght
  answers <- klsr@answ
  correct <- klsr@corr

  # for a nice printout, check numer of needed digits for points.
  # e.g, if you can get 1/2 points, you'd need one digit. but we won't allow more than two!
  if(identical(round(res.points[,-1], digits=0), res.points[,-1])){
    print.digits <- 0
  } else if(identical(round(res.points[,-1], digits=1), res.points[,-1])){
    print.digits <- 1
  } else {
    print.digits <- 2
  }

### functions were here!

  if(identical(matn, "all")){
    for(i in results$MatrNo){
      tabellenbau(matn=i, res.points=res.points, results=results, answers=answers, correct=correct, klsr=klsr,
        text=text, descr=descr, marks.info=marks.info, hist=hist, hist.points=hist.points, hist.marks=hist.marks, NRET.legend=NRET.legend,
        print.digits=print.digits, merge=merge, alt.candy=alt.candy, table.size=table.size, file.name=file.name,
        save=save, pdf=pdf, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
    }
    if(isTRUE(merge) & isTRUE(pdf)){
      merge.reports(results=results, text=text, descr=descr, file.name=file.name, pdf=pdf, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
    } else {}
  } else if(identical(matn, "anon")){
    global.report(form="anon", klsr=klsr, text=text, save=save, pdf=pdf, anon.glob.file=anon.glob.file, print.digits=print.digits, table.size=table.size,
      hist=hist, marks.info=marks.info, descr=descr, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
  } else if(identical(matn, "glob")){
    global.report(form="global", klsr=klsr, text=text, save=save, pdf=pdf, anon.glob.file=anon.glob.file, print.digits=print.digits, table.size=table.size,
      hist=hist, marks.info=marks.info, descr=descr, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
  } else {
    tabellenbau(matn=matn, res.points=res.points, results=results, answers=answers, correct=correct, klsr=klsr,
      text=text, descr=descr, marks.info=marks.info, hist=hist, hist.points=hist.points, hist.marks=hist.marks, NRET.legend=NRET.legend,
      print.digits=print.digits, merge=merge, alt.candy=alt.candy, table.size=table.size, file.name=file.name,
      save=save, pdf=pdf, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
  }
}
