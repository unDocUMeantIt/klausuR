#' \code{klausur.report} takes (at least) an object of class klausuR and a matriculation number to generate personal test results
#' in LaTeX and/or PDF format.
#'
#'The report contains, next to the individual results, a table with all given and correct  answers (using \code{\link[xtable]{xtable}}),
#' as well as nice histograms showing the distribution of the test results (points and/or marks are supportet). If the matriculation numer
#' is set to "all", reports for all subjects are produced. Setting it to "anon" will get you a printable version of the anonymized results.
#' 
#' By default output is sent to standard out. To save them to disk in LaTeX format a "save" parameter is provided. Alternatively, the reports
#' can be converted to PDF format as well. \code{klausur.report} is calling \code{\link[texi2dvi]{texi2dvi}} from the \code{tools} package for that.
#'
#' @title Generate individual reports on multipe choice test results
#' @method klausur.report klausuR
#' @usage
#' klausur.report(klsr, matn, save=FALSE, pdf=FALSE, path=NULL, file.name="matn",
#'  hist=list(points=FALSE, marks=FALSE),
#'  hist.points="hist_points.pdf", hist.marks="hist_marks.pdf",
#'  marks.info=list(points=FALSE, percent=FALSE),
#'  descr=list(title=NULL, name=NULL, date=NULL),
#'  lang="en", alt.candy=TRUE, anon.glob.file="anon.tex")
#' @param klsr An object of class klausuR.
#' @param matn Matriculation number, "all" (produces individuall documents for all subjects), "anon" (produces anonymous feedback)
#'	or "glob" (produces a global results document).
#' @param save logical: If TRUE, files are saved to disk (scheme: "\code{path}/\code{matn}.tex").
#' @param pdf logical: If TRUE, LaTeX reports will be converted to PDF automatically, using \code{\link[texi2dvi]{texi2dvi}}.
#'	If \code{save} is FALSE, a temporary directory is used, that is only the PDF files will be saved.
#' @param path Path for \code{save} and \code{hist} files.
#' @param file.name File name scheme for the reports, either "matn" (matriculation number) or "name" (name and firstname).
#' @param hist A list with the logical elements \code{points} and \code{marks}: If TRUE, the reports will include histograms
#'	of the distribution of points and/or marks. The needed PDF files will be created by \code{\link[plot,klausuR]{plot}} and saved as well.
#'	(see \code{path}, \code{hist.points} and \code{hist.marks}).
#' @param hist.points File name for the histogram of points.
#' @param hist.marks File name for the histogram of marks.
#' @param descr Details on the test: List with the elements \code{title} (title of the test), \code{name} (your name) and \code{date}.
#' @param marks.info A list with the logical elements \code{points} and \code{percent}: If TRUE, the reports will include a table showing
#'	how marks were assigned to points achieved and/or percent solved, respectively.
#' @param lang Set to "de" for reports in German, English is the default.
#' @param alt.candy If TRUE, a comma will be inserted for items with multiple alternatives ("235" becomes "2, 3, 5" in the printout)
#' @param anon.glob.file If \code{matn="anon"} or \code{matn="glob"}, you can specify a filename for this particular report.
#' @aliases klausur.report
#' @keywords IO file
#' @return One or several LaTeX documents. If defined two histograms will be plotted.
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @seealso \code{\link[klausuR:klausur]{klausur}}, \code{\link[xtable]{xtable}}, \code{\link[texi2dvi]{texi2dvi}}
#' @import xtable graphics tools
#' @export
#' @examples
#' data(antworten)
#' 
#' # vector with correct answers:
#' richtig <- c(Item01=3, Item02=2, Item03=2, Item04=2, Item05=4,
#'	Item06=3, Item07=4, Item08=1, Item09=2, Item10=2, Item11=4,
#'	Item12=4, Item13=2, Item14=3, Item15=2, Item16=3, Item17=4,
#'	Item18=4, Item19=3, Item20=5, Item21=3, Item22=3, Item23=1,
#'	Item24=3, Item25=1, Item26=3, Item27=5, Item28=3, Item29=4,
#'	Item30=4, Item31=13, Item32=234)
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
#' klsr.obj <- klausur(answ=antworten, corr=richtig, marks=notenschluessel)
#' klausur.report(klsr=klsr.obj, matn="all", descr=list(title="Klausur Tatort",
#' 	name="Dr. T. Aeter", date="24.09.2010"))

klausur.report <- function(klsr, matn, save=FALSE, pdf=FALSE, path=NULL, file.name="matn",
			    hist=list(points=FALSE, marks=FALSE), hist.points="hist_points.pdf", hist.marks="hist_marks.pdf",
			    descr=list(title=NULL, name=NULL, date=NULL), marks.info=list(points=FALSE, percent=FALSE),
			    lang="en", alt.candy=TRUE, anon.glob.file="anon.tex"){

  # check whether klsr is an object og class "klausuR"
  if(!inherits(klsr, "klausuR"))
    stop(simpleError("The given object is not of class \"klausuR\"!"))

  if(!is.numeric(matn) && matn != "all" && matn != "anon" && matn != "glob")
    stop(simpleError("Value assigned to matn must be numeric, \"all\", \"anon\" or \"glob\"!"))

  if((save == TRUE || pdf == TRUE || hist$points == TRUE || hist$marks == TRUE) && is.null(path))
    stop(simpleError("Files have to be saved, but path is empty!"))

  ## path handling
  # check if path exists
  if(file.info(path)$isdir == FALSE)
    stop(simpleError(paste(path,"is not a valid path!")))
  # PDF creation will be done in an temporal directory if "save" is FALSE
  # we'll make "path" "path.orig" and override it with that tempdir internally
  if(save == FALSE && pdf == TRUE){
    path.orig <- path
    path <- tempfile("klausuR")
      if(!dir.create(path, recursive=TRUE)) stop(simpleError("Couldn't create temporary directory! Try with save=TRUE"))
    # if the function is done, remove the tempdir
    on.exit(if(path != path.orig) unlink(path, recursive=TRUE))
  } else {}
  # to be sure path is treated as a directory, add a trailing slash if omitted
  if(length(grep("/$", path)) == 0)
    path <- paste(path,"/", sep="")

  # define the text of the LaTeX document...
  if(lang == "de"){
  text <- list(Auswertung="Einzelauswertung",
	    DozentIn="Dozent",
	    MatrikelNr="Matrikel-Nr.",
	    Datum="Datum der Klausur",
	    Antwort="Antwort",
	    Korrekt="Korrekt",
	    Punkte="Punkte",
	    Ergebnisse="Ergebnisse",
	    Erreicht="Erreichte Punktzahl",
	    Prozent=" korrekte Antworten",
	    Note="Daraus ergibt sich die \\textbf{Note",
	    VertErgs="Verteilung der Klausurergebnisse",
	    Pseudonym="Pseudonym",
	    AProzent="Prozent",
	    ANote="Note",
	    LfdNr="Nr.",
	    GMatNr="MatrNr.",
	    Name="Name",
	    Vorname="Vorname",
	    Notenschluessel="Notenschlüssel")
  # ... and that of the plots
  hist.text <- list(P.xlab="Punkte",P.ylab="Häufigkeit",P.main="Verteilung nach Punkten",
		    N.xlab="Note",N.ylab="Häufigkeit",N.main="Verteilung nach Noten")
  } ## end of german l10n
  else {
  text <- list(Auswertung="Individual Report",
	    DozentIn="Docent",
	    MatrikelNr="Matriculation No.",
	    Datum="Date of test",
	    Antwort="Answer",
	    Korrekt="Correct",
	    Punkte="Points",
	    Ergebnisse="Results",
	    Erreicht="Achieved score",
	    Prozent=" answers correct",
	    Note="This score gives \\textbf{mark",
	    VertErgs="Distribution of test results",
	    Pseudonym="Pseudonym",
	    AProzent="Percent",
	    ANote="Mark",
	    LfdNr="No.",
	    GMatNr="MatrNo.",
	    Name="Name",
	    Vorname="First name",
	    Notenschluessel="Marks defined")
  # ... and that of the plots
  hist.text <- list(P.xlab="Points",P.ylab="Frequency",P.main="Distribution by points",
		    N.xlab="Marks",N.ylab="Frequency",N.main="Distribution by marks")
  }

  if(hist$points || hist$marks) {
    if(hist$points) {
      pdf(file = paste(path,hist.points,sep=""),
	  width = 10, height = 10,
	  pointsize = 22, bg = "white")
	plot(klsr, xlab=hist.text$P.xlab, ylab=hist.text$P.ylab, main=hist.text$P.main)
      dev.off()
    }

    if(hist$marks) {
      pdf(file = paste(path,hist.marks,sep=""),
	  width = 10, height = 10,
	  pointsize = 22, bg = "white")
	plot(klsr, marks=TRUE, xlab=hist.text$N.xlab, ylab=hist.text$N.ylab, main=hist.text$N.main)
      dev.off()
    }
  }

    ## let's grab some info out of the klausuR-object for code readability
    results <- klsr@results
    truefalse <- klsr@trfls
    answers <- klsr@answ
    correct <- klsr@corr
    # if informatin on marks is wanted, only grab the intended stuff
    if(sum(unlist(marks.info)) > 0){
      marks.information <- as.matrix(klsr@marks.sum[,unlist(marks.info)])
      colnames(marks.information) <- c(text$Punkte, text$AProzent)[unlist(marks.info)]
    }

  # this function will replace German umlauts with LaTeX equivalents
  # some sanitizing is also done
  # it's used in tabellenbau() below
  latex.umlaute <- function(input){
    output <- gsub("ß","\\\\ss{}",as.character(input))
    output <- gsub("ö","\\\\\"o",as.character(output))
    output <- gsub("ü","\\\\\"u",as.character(output))
    output <- gsub("ä",'\\\\\"a',as.character(output))
    output <- gsub("Ö","\\\\\"O",as.character(output))
    output <- gsub("Ü","\\\\\"U",as.character(output))
    output <- gsub("Ä",'\\\\\"A',as.character(output))
    output <- gsub("&",'\\\\&',as.character(output))
    output <- gsub("_",'\\\\_',as.character(output))
    return(output)
  }

  # this function will convert LaTeX to PDF
  # it is called in global.report() and tabellenbau()
  create.pdf <- function(file, path){
      # save current working directory; unfortuneately, texi2dvi() doesn't seem to be able
      # to create PDF files anywhere but in the WD, so we'll have to cd there and back, afterwards
      current.wd <- getwd()
      # change to destined directory
      setwd(path)
      if(file=="all"){
	for(i in dir(pattern="*.tex")) texi2dvi(i, pdf=TRUE, clean=TRUE)
      }
      else{
	texi2dvi(file, pdf=TRUE, clean=TRUE)
      }
      # in case save was FALSE, move the PDFs to the actual destination
      if(save == FALSE && file.info(path.orig)$isdir == TRUE){
	file.copy(dir(pattern="*.pdf"), path.orig, overwrite=TRUE)
      } else {}
      # get back to where we came from
      setwd(current.wd)
  } ## end function create.pdf()

  tabellenbau <- function(matn){
      # TRUE/FALSE-matrix of given answers
      trfl.mtrx <- truefalse[truefalse$MatrNo==matn,]
      einzelergebnis <- results[results$MatrNo==matn,]
      geg.items <- grep("Item([[:digit:]]{1,3})",names(trfl.mtrx))
      geg.trfl <- as.numeric(trfl.mtrx[,geg.items])
      # the indices of all answers
      items <- grep("Item([[:digit:]]{1,3})",names(answers))
      if(alt.candy){
	# some eye-candy: put commata between multiple answer alternatives, if applicable
	# calls the internal function answ.alternatives()
	geg.antw1 <- answ.alternatives(answers[answers$MatrNo==matn,items], latex=TRUE)
	loesungen <- answ.alternatives(correct, latex=TRUE)
      } else {
	geg.antw1 <- answers[answers$MatrNo==matn,items]
	loesungen <- correct
      }

      # name and first name from the answer matrix
      name <- latex.umlaute(einzelergebnis$Name)
      vorname <- latex.umlaute(einzelergebnis$FirstName)
      # points and percent of the results
      punkte <- einzelergebnis$Points
      prozent <- einzelergebnis$Percent
      note <- einzelergebnis$Mark
      # check for file name scheme
      if(file.name == "name")
	name.scheme <- paste(gsub("[[:space:]]", "_", paste(einzelergebnis$Name, einzelergebnis$FirstName)),".tex", sep="")
      else
	name.scheme <- paste(matn,".tex", sep="")
      # create filename from name scheme
      if(save == TRUE || pdf == TRUE)
	dateiname <- paste(path,name.scheme, sep="")
      else
	dateiname <- ""

      # use paste to create the LaTeX head, that is up to the table
      latex.head <- paste("\\documentclass[a4paper,ngerman]{scrartcl}
	    \\usepackage{longtable}
	    \\usepackage{mathptmx}
	    \\usepackage{helvet}
	    \\usepackage{courier}
	    \\usepackage[T1]{fontenc}
	    \\usepackage[latin9]{inputenc}
	      \\setlength{\\parskip}{\\medskipamount}
	      \\setlength{\\parindent}{0pt}
	    \\usepackage{amsmath}
	    \\usepackage{graphicx}
	    \\usepackage{amssymb}
	    \\usepackage{thumbpdf}
	    \\usepackage{color}
	      \\definecolor{dunkelgrau}{gray}{.5}
	      \\definecolor{hellgrau}{gray}{.7}
	      \\definecolor{dunkelblau}{rgb}{.1,.1,.6}
	    \\usepackage[
	      a4paper,
	      pdftex,
	      bookmarksopen,
	      bookmarksopenlevel=1,
	      colorlinks,
	      anchorcolor=dunkelgrau,
	      linkcolor=dunkelgrau,
	      urlcolor=hellgrau,
	      citecolor=dunkelblau]{hyperref}
	    \\usepackage{babel}
	    \\title{",latex.umlaute(descr$title),"}
	    \\date{",latex.umlaute(text$DozentIn),": ",latex.umlaute(descr$name),"\\\\",text$Datum,": ",descr$date,"}
	    \\author{",vorname," ",name,",\\\\",text$MatrikelNr," ",matn,"}
	    \\begin{document}\n\\maketitle\n\\section*{",text$Ergebnisse,"}\n",
	    text$Erreicht,": \\textbf{",punkte,"} (\\textbf{",prozent,"\\%}",text$Prozent,").
	    \\\\",text$Note,": ",format(note, nsmall=1),"}",
	  if(hist$points || hist$marks) {paste("
		\\begin{figure}[h]
		 \\centerline{",
		if(hist$points) {paste("\\mbox{\\includegraphics[width=9cm]{",hist.points,"}}", sep="")},
		if(hist$marks)  {paste("\\mbox{\\includegraphics[width=9cm]{",hist.marks,"}}", sep="")},
		"}
		    \\caption{",text$VertErgs,"}
		\\end{figure}", sep="")},"
		\\newpage",
	    sep="")

      # here comes the foot, that is after the table
      latex.foot <- paste("
		\\end{document}\n",
		sep="")

      # combine parts to a document
      write(latex.head, file=dateiname)
	# create table, the vector "richtig" from function ausw.daten is used
	pre.erg.tabelle <- rbind(geg.antw1,loesungen,geg.trfl)
	rownames(pre.erg.tabelle) <- c(text$Antwort,text$Korrekt,text$Punkte)
	colnames(pre.erg.tabelle) <- names(answers)[items]
      print(xtable(t(pre.erg.tabelle), digits=0,
	caption=paste(text$Auswertung," ",vorname," ",name," (",text$MatrikelNr," ",matn,")",sep="")),
	file=dateiname, appen=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, tabular.environment="longtable", floating=FALSE)
      if(sum(unlist(marks.info)) > 0){
	# summary on the defined marks
	print(xtable(marks.information, caption=latex.umlaute(text$Notenschluessel)),
	  file=dateiname, appen=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, floating=TRUE)
      } else {}
      write(latex.foot, file=dateiname, append=TRUE)

      # check if PDF creation is demanded
      if(pdf == TRUE && is.character(name.scheme)){
	create.pdf(file=name.scheme, path=path)
      } else {}
  } ## end function tabellenbau()

  global.report <- function(form){
      # set the file name
      if((save == TRUE || pdf == TRUE) && is.character(anon.glob.file))
	dateiname <- paste(path, anon.glob.file, sep="")
      else
	dateiname <- ""

      # prepare the table
      if(form == "anon"){
	anon.glob.table <- klsr@anon
	colnames(anon.glob.table) <- c(text$Pseudonym,text$Punkte,text$AProzent,text$ANote)
	anon.glob.digits <- c(0,0,0,1,1)
      }
      else {
	anon.glob.table <- klsr@results
	colnames(anon.glob.table) <- c((if(!is.null(anon.glob.table$No)) text$LfdNr),text$Name,text$Vorname,text$GMatNr,text$Punkte,text$AProzent,text$ANote,(if(!is.null(anon.glob.table$Pseudonym)) text$Pseudonym))
	anon.glob.digits <- c(0,(if(!is.null(anon.glob.table$No)) 0),0,0,0,0,1,1,(if(!is.null(anon.glob.table$Pseudonym)) 0))
      }

      # prepare the LaTeX code
      latex.head <- paste("\\documentclass[a4paper,ngerman]{scrartcl}
	    \\usepackage{longtable}
	    \\usepackage{mathptmx}
	    \\usepackage{helvet}
	    \\usepackage{courier}
	    \\usepackage[T1]{fontenc}
	    \\usepackage[latin9]{inputenc}
	      \\setlength{\\parskip}{\\medskipamount}
	      \\setlength{\\parindent}{0pt}
	    \\usepackage{amsmath}
	    \\usepackage{graphicx}
	    \\usepackage{amssymb}
	    \\usepackage{thumbpdf}
	    \\usepackage{color}
	      \\definecolor{dunkelgrau}{gray}{.5}
	      \\definecolor{hellgrau}{gray}{.7}
	      \\definecolor{dunkelblau}{rgb}{.1,.1,.6}
	    \\usepackage[
	      a4paper,
	      pdftex,
	      bookmarksopen,
	      bookmarksopenlevel=1,
	      colorlinks,
	      anchorcolor=dunkelgrau,
	      linkcolor=dunkelgrau,
	      urlcolor=hellgrau,
	      citecolor=dunkelblau]{hyperref}
	    \\usepackage{babel}
	    \\title{",latex.umlaute(descr$title),"}
	    \\date{",text$Datum,": ",descr$date,"}
	    \\author{",latex.umlaute(text$DozentIn),": ",latex.umlaute(descr$name),"}
	    \\begin{document}\n\\maketitle\n\\section*{",text$Ergebnisse,"}\n",
	    sep="")

      latex.foot <- paste(
	  if(hist$points || hist$marks) {paste("
		\\begin{figure}[h]
		 \\centerline{",
		if(hist$points) {paste("\\mbox{\\includegraphics[width=9cm]{",hist.points,"}}", sep="")},
		if(hist$marks)  {paste("\\mbox{\\includegraphics[width=9cm]{",hist.marks,"}}", sep="")},
		"}
		    \\caption{",text$VertErgs,"}
		\\end{figure}", sep="")},"
		\\end{document}\n",
		sep="")
      # combine parts to a document
      write(latex.head, file=dateiname)
      # create table with anonymous feedback
      print(xtable(anon.glob.table, digits=anon.glob.digits,
	caption=paste(text$Ergebnisse,": ",latex.umlaute(descr$title)," (",latex.umlaute(descr$name),", ",descr$date,")", sep="")),
	file=dateiname, appen=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, tabular.environment="longtable", floating=FALSE)
      if(sum(unlist(marks.info)) > 0){
	# summary on the defined marks
	print(xtable(marks.information, caption=latex.umlaute(text$Notenschluessel)),
	  file=dateiname, appen=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, floating=TRUE)
      } else {}
      write(latex.foot, file=dateiname, append=TRUE)

      # check if PDF creation is demanded
      if(pdf == TRUE && is.character(anon.glob.file)){
	create.pdf(file=anon.glob.file, path=path)
      } else {}
  } ## end function global.report()

  if(matn=="all"){
    for(i in results$MatrNo) tabellenbau(matn=i)
  }
  else if(matn=="anon"){
    global.report(form="anon")
  }
  else if(matn=="glob"){
    global.report(form="global")
  }
  else
    tabellenbau(matn)
}
