\name{NEWS}
\title{News for Package 'klausuR'}
\encoding{UTF-8}
\section{Changes in klausuR version 0.12-9 (2013-08-05)}{
  \subsection{fixed}{
    \itemize{
      \item pdfLaTeX became picky and didn't work with the table of marks any more.
        set \code{print.xtable()} to use floating=FALSE to fix this.
      \item added some more character filtering on generated filenames which could
        cause LaTeX to fail with an error
    }
  }
}
\section{Changes in klausuR version 0.12-8 (2013-05-08)}{
  \subsection{changed}{
    \itemize{
      \item no real new features, restructured the source code layout, as roxygen2
        doesn't seem to get collate info easy with R 3.0.0 ...
    }
  }
}
\section{Changes in klausuR version 0.12-7 (2013-01-25)}{
  \subsection{fixed}{
    \itemize{
      \item corrected LaTeX output (carets)
      \item added missing date stamps in ChangeLog for releases prior to version
        0.12-1, using the repo archive
    }
  }
  \subsection{added}{
    \itemize{
      \item item analysis now includes Lienert's selection index (selektionskennwert)
      \item debianized sources
    }
  }
}
\section{Changes in klausuR version 0.12-6 (2012-04-14)}{
  \subsection{fixed}{
    \itemize{
      \item check for missing items in data and/or correct answers
      \item force item names into expected letter case
      \item corrected LaTeX output (filtering of problematic characters)
    }
  }
}
\section{Changes in klausuR version 0.12-5 (2012-03-10)}{
  \itemize{
    \item replaced custom function to clean up LaTeX code with
      \code{encoded_text_to_latex()} from tools package in \code{klausur.report()}
    \item replaced UTF-8 characters with \\uxxxx escapes in \code{klausur.report()}
    \item set some internal variables NULL in \code{compare()}, \code{grand.table()},
      \code{klausur.gen()}, \code{klausur.mufo()}, \code{klausur.report()} and the internal function
      \code{klausur.reorderItems()}, to avoid needless NOTEs from "R CMD check"
    \item re-formatted the ChangeLog to have the roxyPackage translate it into a
      NEWS.Rd file
  }
}
\section{Changes in klausuR version 0.12-4 (2012-02-09)}{
  \itemize{
    \item fixed bug in \code{compare()}: \code{"new.set"} didn't work if exactly one or none
      differences were found
    \item added 'maxp' option to klausur.answ-class and \code{klausur.date()}, to be able
      to compensate for really buggy tests...
    \item adjusted \code{klausur()} to check for maxp
  }
}
\section{Changes in klausuR version 0.12-3 (2011-10-04)}{
  \itemize{
    \item added option \code{"merge"} to \code{klausur.report()}, to merge all individual reports
      into one file
    \item added status feedback to  \code{klausur.report()}, can be turned of with new
      option "quiet=TRUE"
    \item updated the RKWard GUI to the new options
  }
}
\section{Changes in klausuR version 0.12-2 (2011-09-11)}{
  \itemize{
    \item fixed bug in \code{nret.rescale()} regarding NR data
    \item fixed author definition for R 2.14
  }
}
\section{Changes in klausuR version 0.12-1 (2011-09-01)}{
  \itemize{
    \item cleaned the docs so package check runs fine
    \item fixed typo in \code{klausur.report()}
  }
}
\section{Changes in klausuR version 0.12-0 (2011-08-30)}{
  \itemize{
    \item 0.12 is not fully downward compatible with earlier versions of the
      klausuR package!
    \item drastic changes: \code{klausur()} and \code{klausur.mufo()} from now on only take
      objects of the new class klausur.answ, which replaces the 'answ' and 'corr'
      arguments completely. Most of the time it took to evaluate tests was
      squeezing data with exotic variable names and loads of surprises into a valid
      data.frame. Those days should finally be numbered now...
    \item added function \code{klausur.data()} to do the squeezing into klausur.answ class
      objects (s.o.)
    \item added scoring function NRET+, which is more conservative than NRET when
      it comes to errors
    \item added function \code{nret.rescale()} to change scaling of points in result
      objects (NR/ET/NRET/NRET+)
    \item added function \code{grand.table()} to export all results back to CSV format
    \item the class klausuR gained a new slot 'misc' to preserve whatever unused
      variables are left over from the squeezing process.
    \item introduced class klausuR.test, now as a placeholder for future releases,
      and added slots to classes klausuR and klausuR.answ
    \item added function \code{nret.translator()} to convert NRET/ET data from/to SPSS
    \item added options 'table.size' and 'NRET.legend' to \code{klausur.report()}
    \item smarter handling of histograms and marks info in \code{klausur.report()}
    \item \code{klausur.report()} can now also create histograms with data from several
      klausuR objects
    \item \code{klausur.report()} can now produce reports from more than one object, if
      you give them in a list
    \item \code{klausur.compare()} was renamed to \code{compare()} and can now cope with NAs,
      unequal cases and unequal variables (will cause verbose warnings), and rename
      input variables, if necessary. it is also no longer limited to compare
      klausuR data, hence the renaming.
    \item fixed some NRET/ET related bugs in the plot method and \code{klausur.gen.marks()}
    \item fixed 'if' statement error in show method for klausuR-class objects
    \item added support for item analysis on ET/NRET data
    \item added NRET sample data \code{"spss.data"}
    \item added SD values to item analysis (show method)
    \item added 'sort.by' option to \code{klausur.data()}, and \code{klausur()}
    \item added sort method for objects of class klausuR
    \item added the full RKWard plugin as inst/rkward, so both get updated
      simultanously
    \item adapted rkwardtests to current behaviour of klausuR
    \item sources can now be roxygenized by roxygen2 (classes docs are static until
      '@slot' is supported again)
  }
}
\section{Changes in klausuR version 0.11-3 (2011-06-19)}{
  \itemize{
    \item aligned penalty for false negatives to number of correct alternatives in
      NRET/ET scoring
    \item for NRET/ET, moved determination of max. points etc. to new internal
      function \code{nret.minmax()}
  }
}
\section{Changes in klausuR version 0.11-2 (2011-06-17)}{
  \itemize{
    \item added internal function \code{roxy.package()} to ease development
    \item added initial support for elimination testing (ET) and number right
      elimination testing (NRET)
    \item fixed \code{"Form"} addition in \code{klausur.gen()}
  }
}
\section{Changes in klausuR version 0.11-1 (2011-04-09)}{
  \itemize{
    \item fixed bug in points (were logical instead of numeric, which caused
      trouble for PDF generation)
    \item fixed annoying \code{"show"} warning
  }
}
\section{Changes in klausuR version 0.10-1 (2011-02-14)}{
  \itemize{
    \item complete rewrite of \code{klausur.compare()}: less and more efficient code
    \item replaced \code{"na.replace"} option with a plain \code{"na.rm"}, i.e., NA cases are
      dropped completely by default
    \item added new \code{"na.rm"} option to plot method, TRUE by default
    \item added variable \code{"Form"} to \code{klausur.gen()}
    \item replaced several "if(a == b)" constructs with "identical(a, b)" or
      "isTRUE(...)"
    \item fixed: \code{klausur.report()} accidently created PDFs in a loop over and over
      again
    \item fixed: \code{klausur.report()} failed if "#" appeared in names
    \item fixed: \code{klausur.report()} didn't show partial results correctly, added
      check for needed digits
    \item added slots \code{"wght"} and \code{"points"} to class klausuR
  }
}
\section{Changes in klausuR version 0.10-0 (2010-11-03)}{
  \itemize{
    \item added \code{klausur.mufo()} and new S4 class klausuR.mult: initial support for
      parallel test forms
    \item added \code{show()} and \code{plot()} methods for objects of class klausuR.mult
    \item added support for objects of class klausuR.mult in \code{klausur.report()}
    \item fixed missing import of tools package
    \item dropped roxygen dependency (still using roxygen, though)
    \item some code cleanup for better readability
  }
}
\section{Changes in klausuR version 0.9-7 (2010-09-24)}{
  \itemize{
    \item added direct PDF creation to \code{klausur.report()}, using \code{texi2dvi()} from
      package tools
    \item consequently added the tools package as a dependency
  }
}
\section{Changes in klausuR version 0.9-6 (2010-09-24)}{
  \itemize{
    \item added schemes \code{"DIHK"}, \code{"USA"} and \code{"UK"} to \code{klausuR.gen.marks()}
    \item added informative slot \code{"marks.sum"} to objects of class klausuR
    \item updated the show method for klausuR objects accordingly
    \item added \code{"marks.info"} option to \code{klausur.report()}, to include all or parts of
      \code{"marks.sum"} if wanted
  }
}
\section{Changes in klausuR version 0.9-5 (2010-08-07)}{
  \itemize{
    \item fixed another bug in \code{klausur.compare()}: objects with NAs meant trouble.
    \item added \code{"select"} option to \code{klausur.compare()} to be able to focus on certain
      variables
  }
}
\section{Changes in klausuR version 0.9-4 (2010-08-06)}{
  \itemize{
    \item added support for different file name scheme to \code{klausur.report()} (matr.
      no. or name_firstname)
  }
}
\section{Changes in klausuR version 0.9-3 (2010-02-04)}{
  \itemize{
    \item added \code{"alt.comma"} parameter to \code{klausur.report()}
    \item minor corrections in the documentation (some missing params in usage)
  }
}
\section{Changes in klausuR version 0.9-2 (2009-12-04)}{
  \itemize{
    \item restructured the LaTeX output
    \item fixed some bugs in \code{klausur.compare()}
    \item added parameters to skip cronbach's alpha and item analysis in \code{klausur()}
    \item cronbach's alpha and item analysis now run in a safe \code{tryCatch()}
      environment
    \item modified the \code{show()} method accordingly
  }
}
\section{Changes in klausuR version 0.9-1 (2009-12-02)}{
  \itemize{
    \item added "alpha if removed" calculation for each item
    \item added longtable support for \code{klausur.report()}
    \item fixed some bugs in \code{klausur()}, \code{klausur.report()} and \code{plot()} regarding
      character vectors
  }
}
\section{Changes in klausuR version 0.9-0 (2009-11-27)}{
  \itemize{
    \item added support for partially correct answers (if multiple correct answers
      possible) via the new \code{"score"} parameter
    \item more cleaning up: moved some subroutines to internal functions
  }
}
\section{Changes in klausuR version 0.8-1 (2009-11-20)}{
  \itemize{
    \item added marks A to F as an option in \code{klausur.gen.marks()}
    \item adjusted the plot method to treat marks as characters
    \item beautyfied the output of individual results in \code{klausur()}
    \item cleaned up a bit (removed unused relicts from klausur.R)
    \item translated hopefully all comments in the code into english
  }
}
\section{Changes in klausuR version 0.8-0 (2009-11-19)}{
  \itemize{
    \item added feature to suggest marks by normal distribution
  }
}
\section{Changes in klausuR version 0.7-0 (2009-11-06)}{
  \itemize{
    \item initial public release (via reaktanz.de)
  }
}
