# Copyright 2009-2022 Meik Michalke <meik.michalke@hhu.de>
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

## function latex.head()
latex.head <- function(text, one.file=FALSE, individual=TRUE, hist.and.marks=FALSE, marks.hist.stuff=NULL, person=list(),
  descr=list(title=NULL, name=NULL, date=NULL), fancyhdr=TRUE, landscape=FALSE){
  # use paste to create the LaTeX head, that is up to the table
  full.head <- paste0("\\documentclass[a4paper,ngerman", ifelse(isTRUE(landscape), ",landscape", ""),"]{scrartcl}\n",
    ifelse(isTRUE(hist.and.marks), "      \\usepackage[a4paper,hmargin={2cm,2cm}]{geometry}\n", ""),
    "      \\usepackage{longtable}
    \\usepackage{mathptmx}
    \\usepackage{helvet}
    \\usepackage{courier}
    \\usepackage[T1]{fontenc}
    \\usepackage[latin9]{inputenc}
      \\setlength{\\parskip}{\\medskipamount}
      \\setlength{\\parindent}{0pt}",
    if(isTRUE(fancyhdr)){
      paste0(
        "      \\usepackage{fancyhdr}\n",
        "      \\pagestyle{fancy}\n",
        "      \\fancyhf{} % clean headers\n",
        "      \\fancyfoot[EC,OC]{\\thepage}\n",
        if(isTRUE(individual)){
          paste0(
            "      \\fancyfoot[OR,ER]{",person$vorname," ",person$name,"}\n",
            "      \\fancyfoot[EL,OL]{",person$matn,"}\n"
          )
        } else {
          paste0(
            "      \\fancyfoot[OR,ER]{",latex.umlaute(text$DozentIn),"}\n",
            "      \\fancyfoot[OL,EL]{",descr$date,"}\n"
          )
        },
        "      \\fancyhead[EC,OC]{",latex.umlaute(descr$title),"}\n",
        "      \\renewcommand{\\headrulewidth}{0pt}\n"
      )
    } else {},
    "      \\usepackage{amsmath}
    \\usepackage{graphicx}\n",
    ifelse(isTRUE(one.file), "      \\usepackage{pdfpages}\n", ""),
    "      \\usepackage{amssymb}
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
    \\title{",latex.umlaute(descr$title),"}\n",
    if(isTRUE(individual)){
      # this is for individual reports, so each subject's name is printed
      paste0("      \\date{",latex.umlaute(text$DozentIn),": ",latex.umlaute(descr$name),"\\\\",text$Datum,": ",descr$date,"}
    \\author{",person$vorname," ",person$name,",\\\\",text$MatrikelNr," ",person$matn,"}\n")
    } else {
      # anonymous or global results
      paste0("      \\date{",text$Datum,": ",descr$date,"}
    \\author{",latex.umlaute(text$DozentIn),": ",latex.umlaute(descr$name),"}\n")
    },
    if(isTRUE(individual)){
      # this is for individual reports
      paste0("\\begin{document}\n\\maketitle\n\\section*{",text$Ergebnisse,"}\n",
      text$Erreicht,": \\textbf{",person$punkte,"} (\\textbf{",person$prozent,"\\%}",text$Prozent,").
      \\\\",text$Note,": ",format(person$note, nsmall=1),"}
      ",
      marks.hist.stuff,"
        \\newpage\n")
    } else if(isTRUE(one.file)){
      # combine all individual reports into one file
      paste0("\\begin{document}\n\\maketitle\n")
    } else {
      # anonymous or global results
      paste0("\\begin{document}\n\\maketitle\n\\section*{",text$Ergebnisse,"}\n")
    }
  )
  return(full.head)
} ## end function latex.head()


## function create.pdf()
# this function will convert LaTeX to PDF
# it is called in global.report() and tabellenbau()
# the suppress option is for mergeing files to skip copying the individual reports
create.pdf <- function(file, path, path.orig=path, suppress=FALSE, save=FALSE){
  # save current working directory; unfortuneately, texi2dvi() doesn't seem to be able
  # to create PDF files anywhere but in the WD, so we'll have to cd there and back, afterwards
  current.wd <- getwd()
  # change to destined directory
  setwd(file.path(path))
  texi2dvi(file.path(file), pdf=TRUE, clean=TRUE)
  # in case save and merge were FALSE, move the PDFs to the actual destination
  if(!isTRUE(save) && !isTRUE(suppress) && isTRUE(file.info(path.orig)$isdir)){
    file.copy(gsub(".tex", ".pdf", file), path.orig, overwrite=TRUE)
  } else {}
  # get back to where we came from
  setwd(file.path(current.wd))
} ## end function create.pdf()


## function tabellenbau()
# this is the main function for individual reports
#' @importFrom xtable xtable
#' @importFrom utils capture.output
#' @noRd
tabellenbau <- function(matn, res.points, results, answers, correct, klsr, text, descr=list(title=NULL, name=NULL, date=NULL), marks.info=list(points=FALSE, percent=FALSE),
  hist=list(points=FALSE, marks=FALSE), hist.points="hist_points.pdf", hist.marks="hist_marks.pdf", NRET.legend=FALSE, print.digits=2, merge=FALSE,
  alt.candy=TRUE, table.size="auto", file.name="matn", save=FALSE, pdf=FALSE, path=tempdir(), path.orig=path, quiet=FALSE, fancyhdr=TRUE){
  points.mtrx <- res.points[res.points$MatrNo==matn,]
  einzelergebnis <- results[results$MatrNo==matn,]
  geg.items <- grep("Item([[:digit:]]{1,3})",names(points.mtrx))
  geg.points <- as.numeric(points.mtrx[,geg.items])
  # the indices of all answers
  items <- grep("Item([[:digit:]]{1,3})",names(answers))
  if(isTRUE(alt.candy)){
    # some eye-candy: put commata between multiple answer alternatives, if applicable
    # calls the internal function answ.alternatives()
    geg.antw1 <- answ.alternatives(answers[answers$MatrNo==matn,items], latex=TRUE)
    loesungen <- answ.alternatives(correct, latex=TRUE)
  } else {
    geg.antw1 <- answers[answers$MatrNo==matn,items]
    loesungen <- correct
  }

  # define table size
  if(identical(table.size, "auto")){
    if(length(items) > 37 & length(items) <= 45){
      # to avoid ugly tables with few lines on one page, shrink by heuristics
      table.size <- "\\small\n"
    } else if(length(items) > 45 & length(items) < 50){
      table.size <- "\\footnotesize\n"
    } else {
      table.size <- "\\normalsize\n"
    }
  } else {
    table.size <- paste0("\\",table.size,"\n")
  }

  # name and first name from the answer matrix
  name <- latex.umlaute(einzelergebnis$Name)
  vorname <- latex.umlaute(einzelergebnis$FirstName)
  # points and percent of the results
  punkte <- einzelergebnis$Points
  prozent <- einzelergebnis$Percent
  note <- einzelergebnis$Mark

  if (!isTRUE(quiet)){
    # give some feedback on current status
    message(paste("Processing: ", einzelergebnis$FirstName, " ", einzelergebnis$Name, " (", matn, ")", sep=""))
  } else {}

  # check for file name scheme
  if(identical(file.name, "name")){
    name.scheme <- paste(file.umlaute(gsub("[[:space:]]", "_", paste(einzelergebnis$Name, einzelergebnis$FirstName))),".tex", sep="")
  } else {
    name.scheme <- paste(matn,".tex", sep="")
  }
  # create filename from name scheme
  if(isTRUE(save) || isTRUE(pdf)){
    dateiname <- file.path(path, name.scheme)
  } else {
    dateiname <- ""
  }

  # prepare histograms and/or marks info
  if(any(unlist(marks.info))){
    # if informatin on marks is wanted, only grab the intended stuff
    marks.information <- as.matrix(slot(klsr, "marks.sum")[,unlist(marks.info)])
    colnames(marks.information) <- c(text$Punkte, text$AProzent)[unlist(marks.info)]
    # summary on the defined marks
    # use capture.output() to get rid of the printout; the table tags need to be removed,
    # we need only the tabular environment
    unused.garbage <- capture.output(
        marks.info.table <- print(xtable(marks.information),
        floating=FALSE,
        sanitize.text.function=function(x){latex.umlaute(x)})
      )
    marks.info.tabular <- paste(
      gsub("\\\\end\\{center\\}\\n\\\\end\\{table\\}\\n","",
      gsub("\\\\begin\\{table\\}\\[ht\\]\\n\\\\begin\\{center\\}\\n", "", marks.info.table))
    )
  } else {
    marks.info.table <- ""
    marks.info.tabular <- ""
  }
  # first cases including histograms
  if(any(unlist(hist))){
    if(any(unlist(marks.info))){
      if(sum(unlist(hist)) == 1){
        hist.and.marks <- FALSE
        # one graph and marks
        marks.hist.stuff <- paste("
        \\begin{table}[ht]
          \\begin{minipage}[b]{0.5\\linewidth}
          \\centering
          \\begin{tabular}{c}
            \\includegraphics[width=9cm]{",
            if(hist$points){
              paste(hist.points)
            } else {
              paste(hist.marks)
            },"}
          \\end{tabular}
          \\end{minipage}
          \\hspace{0.8cm}
          \\begin{minipage}[b]{0.5\\linewidth}
            \\small
            \\centering
            ",marks.info.tabular,"
          \\end{minipage}
          \\caption{", latex.umlaute(text$VertErgs), ", ", latex.umlaute(text$Notenschluessel),"}
        \\end{table}%", sep="")
      } else {
        hist.and.marks <- TRUE
        # two graphs and marks
        marks.hist.stuff <- paste("
        \\begin{table}[ht]
          \\begin{minipage}[b]{0.6\\linewidth}
          \\centering
          \\begin{tabular}{c@{\\hskip 0cm}c}
            \\includegraphics[width=0.6\\linewidth]{",hist.points,"}&\\includegraphics[width=0.6\\linewidth]{",hist.marks,"}
          \\end{tabular}
          \\end{minipage}
          \\hspace{0.8cm}
          \\begin{minipage}[b]{0.4\\linewidth}
            \\footnotesize
            \\centering
            ",marks.info.tabular,"
          \\end{minipage}
          \\caption{", latex.umlaute(text$VertErgs), ", ", latex.umlaute(text$Notenschluessel),"}
        \\end{table}%", sep="")
      }
    } else {
      # just one or two graphs
      hist.and.marks <- FALSE
      marks.hist.stuff <- paste("
      \\begin{figure}[h]
      \\centerline{",
      if(hist$points){
        paste("\\mbox{\\includegraphics[width=9cm]{",hist.points,"}}", sep="")
      } else {},
      if(hist$marks){
        paste("\\mbox{\\includegraphics[width=9cm]{",hist.marks,"}}", sep="")
      } else {},
      "}
        \\caption{",text$VertErgs,"}
      \\end{figure}", sep="")
    }
  } else if(any(unlist(marks.info))){
    hist.and.marks <- FALSE
    # cases without histograms but marks
    marks.hist.stuff <- marks.info.table
  } else {
    hist.and.marks <- FALSE
    # cases with neither histograms nor marks
    marks.hist.stuff <- ""
  }

  # here comes the foot, that is after the table
  latex.foot <- paste("
    \\end{document}\n",
  sep="")

  # combine parts to a document
  write(paste(
    latex.head(text=text, one.file=FALSE, individual=TRUE, hist.and.marks=hist.and.marks,
    person=list(vorname=vorname, name=name, matn=matn, punkte=punkte, prozent=prozent, note=note),
    marks.hist.stuff=marks.hist.stuff, descr=descr, fancyhdr=fancyhdr),
    table.size), file=dateiname)
  # create table
  pre.erg.tabelle <- rbind(geg.antw1,loesungen,geg.points)
  rownames(pre.erg.tabelle) <- c(text$Antwort,text$Korrekt,text$Punkte)
  colnames(pre.erg.tabelle) <- names(answers)[items]
  if(isTRUE(NRET.legend)){
    cap.extra <- text$NRET.expl
  } else {
    cap.extra <- ""
  }
  print(xtable(t(pre.erg.tabelle), digits=c(0,0,0,print.digits),
  caption=paste(text$Auswertung," ",vorname," ",name," (",text$MatrikelNr," ",matn,")", cap.extra, sep="")),
  file=dateiname, append=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, tabular.environment="longtable", floating=FALSE)
  write(latex.foot, file=dateiname, append=TRUE)

  # check if PDF creation is demanded
  if(isTRUE(pdf) && is.character(name.scheme)){
    if(isTRUE(merge)){
      suppress=TRUE
    } else {
      suppress=FALSE
    }
    #file, path, path.orig=path, suppress=FALSE, save=FALSE
    create.pdf(file=name.scheme, path=path, path.orig=path.orig, suppress=suppress, save=save)
  } else {}
} ## end function tabellenbau()


## function global.report()
#' @importFrom xtable xtable
#' @noRd
global.report <- function(form, klsr, text, save=FALSE, pdf=FALSE, anon.glob.file="anon.tex", hist.points="hist_points.pdf", hist.marks="hist_marks.pdf",
  print.digits=2, table.size="auto", hist=list(points=FALSE, marks=FALSE), marks.info=list(points=FALSE, percent=FALSE),
  descr=list(title=NULL, name=NULL, date=NULL), path=tempdir(), path.orig=path, quiet=FALSE, fancyhdr=TRUE){
  # set the file name
  if((isTRUE(save) || isTRUE(pdf)) && is.character(anon.glob.file)){
    dateiname <- file.path(path, anon.glob.file)
  } else {
    dateiname <- ""
  }

    # prepare the table
    if(identical(form, "anon")){
      anon.glob.table <- slot(klsr, "anon")
      colnames(anon.glob.table) <- c(text$Pseudonym,text$Punkte,text$AProzent,text$ANote)
      anon.glob.digits <- c(0,0,print.digits,1,1)
      if (!isTRUE(quiet)){
        # give some feedback on current status
        message(paste("Processing: Anonymous feedback...", sep=""))
      } else {}
    } else {
    anon.glob.table <- slot(klsr, "results")
    colnames(anon.glob.table) <- c((if(!is.null(anon.glob.table$No)) text$LfdNr),text$Name,text$Vorname,text$GMatNr,text$Punkte,text$AProzent,text$ANote,(if(!is.null(anon.glob.table$Pseudonym)) text$Pseudonym))
    anon.glob.digits <- c(0,(if(!is.null(anon.glob.table$No)) 0),0,0,0,print.digits,1,1,(if(!is.null(anon.glob.table$Pseudonym)) 0))
    if (!isTRUE(quiet)){
      # give some feedback on current status
      message(paste("Processing: Global results...", sep=""))
    } else {}
    }

  # define table size
  if(identical(table.size, "auto")){
    if((nrow(anon.glob.table) > 25 & nrow(anon.glob.table) < 31)
      | (nrow(anon.glob.table) > 68 & nrow(anon.glob.table) < 78)){
      # to avoid ugly tables with few lines on one page, shrink by heuristics
      table.size <- "\\footnotesize\n"
    } else {
      table.size <- "\\normalsize\n"
    }
  } else {
    table.size <- paste0("\\",table.size,"\n")
  }

  latex.foot <- paste(
  if(hist$points | hist$marks){
    paste("
    \\begin{figure}[h]
    \\centerline{",
    if(hist$points){
      paste("\\mbox{\\includegraphics[width=9cm]{",hist.points,"}}", sep="")
    } else {},
    if(hist$marks){
      paste("\\mbox{\\includegraphics[width=9cm]{",hist.marks,"}}", sep="")
    } else {},
    "}
    \\caption{",text$VertErgs,"}
    \\end{figure}", sep="")
  } else {},"
  \\end{document}\n",
  sep="")
  # combine parts to a document
  write(paste(latex.head(text=text, one.file=FALSE, individual=FALSE, hist.and.marks=FALSE, marks.hist.stuff=NULL,
      descr=descr, fancyhdr=fancyhdr, landscape=identical(form, "global")), table.size), file=dateiname)
  # create table with anonymous feedback
  print(xtable(anon.glob.table, digits=anon.glob.digits,
  caption=paste(text$Ergebnisse,": ",latex.umlaute(descr$title)," (",latex.umlaute(descr$name),", ",descr$date,")", sep="")),
  file=dateiname, append=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, tabular.environment="longtable", floating=FALSE)
## TODO: move construction of marks.information from tabellenbau() to klausur.report() and provide it here
## this code is broken as there is no object called marks.information to be used
#   if(sum(unlist(marks.info)) > 0){
#     # summary on the defined marks
#     print(xtable(marks.information, caption=latex.umlaute(text$Notenschluessel)),
#     file=dateiname, append=TRUE, sanitize.text.function=function(x){latex.umlaute(x)}, floating=TRUE)
#   } else {}
  write(latex.foot, file=dateiname, append=TRUE)

  # check if PDF creation is demanded
  if(isTRUE(pdf) && is.character(anon.glob.file)){
    create.pdf(file=anon.glob.file, path=path, path.orig=path.orig, suppress=FALSE, save=save)
  } else {}
} ## end function global.report()


## function merge.reports()
# creates one PDF file from the individual reports
merge.reports <- function(results, text, descr, file.name="matn", pdf=FALSE, path=tempdir(), path.orig=path, quiet=FALSE, fancyhdr=FALSE){
  merge.file <-  file.path(path, "individual_reports.tex")
  if(identical(file.name, "name")){
    name.scheme <- sapply(results$MatrNo, function(matn){
        einzelergebnis <- results[results$MatrNo==matn,]
        paste(file.umlaute(gsub("[[:space:]]", "_", paste(einzelergebnis$Name, einzelergebnis$FirstName))),".pdf", sep="")
      })
  } else {
    name.scheme <- paste(results$MatrNo,".pdf", sep="")
  }
  # create filename from name scheme
  all.pdf.files <- file.path(path, name.scheme)

  # here comes the foot
  latex.foot <- paste("
    \\end{document}\n",
  sep="")

  # combine parts to a document
  write(paste(
    latex.head(text=text, one.file=TRUE, individual=FALSE, hist.and.marks=FALSE, marks.hist.stuff=NULL,
      descr=descr, fancyhdr=FALSE),
    paste("      \\includepdf[pages=-]{", all.pdf.files, "}", sep="", collapse="\n"),
    "\n",
    latex.foot, sep=""),
    file=merge.file)

  if (!isTRUE(quiet)){
    # give some feedback on current status
    message(paste("Merging individual reports into one file...", sep=""))
  } else {}

  # check if PDF creation is demanded
  if(isTRUE(pdf)){
    create.pdf(file="individual_reports.tex", path=path, path.orig=path.orig, suppress=FALSE, save=save)
  } else {}
} ## end function merge.reports()
