# Copyright 2023 Meik Michalke <meik.michalke@hhu.de>
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

#' Arrange meta data for a test to analyze
#'
#' Creates a list with formatted entries that can be used by functions like \code{\link[klausuR:klausur.data]{klausur.data}}.
#'
#' @param title Character string, title of the test.
#' @param name Character string, name of the lecturer applying the test.
#' @param data_dir Character string, root directory for test data.
#' @param corr Named vector, see \code{corr} argument of \code{\link[klausuR:klausur.data]{klausur.data}}.
#' @param marks Character vector, see \code{marks} argument of \code{\link[klausuR:klausur.data]{klausur.data}}, and
#'    \code{\link[klausuR:klausur.gen.marks]{klausur.gen.marks}}.
#'    If you provide a named numeric vector, the names will be used as mark labels and the numbers as the maximum number of
#'    points for that note. Start with the worst mark (test failed).
#' @param ignore Optional character vector, can be used as the \code{ignore} vector of \code{\link[klausuR:compare]{compare}}.
#' @param subdir_prefix Optional character string. If set, a subdirectory named \code{<subdir_prefix>_<date>} will be
#'    added to both \code{data_dir} and \code{out_dir}, with \code{<date>} being in \code{YYY-MM-DD} format. If \code{out_dir}
#'    is the same as \code{data_dir}, it will have this subdirectory added twice recursively, so reports are easier to archive.
#' @param rename Optional named character vector to use with \code{rename} (see \code{\link[klausuR:klausur.data]{klausur.data}}) if the \code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, \code{Pseudonym}, or \code{Form} column (see \code{\link[klausuR:klausur.mufo]{klausur.mufo}}) has a different name in your data set.
#' @param dummies Optional character vector, see \code{dummies} argument of \code{\link[klausuR:klausur.data]{klausur.data}}.
#' @param disc.misc Logical, see \code{\link[klausuR:klausur.data]{klausur.data}}.
#' @param na.rm Logical, see \code{\link[klausuR:klausur.data]{klausur.data}}.
#' @param item.prefix Character, see \code{\link[klausuR:klausur.data]{klausur.data}}.
#' @param out_dir Character string, root directory for test results.
#' @param date Date of the test in \code{POSIXct} or \code{POSIXlt} format. Defaults to the current date if not set.
#'    Use something like \code{strptime("YYYY-MM-DD", format="%Y-%m-%d")} (with "YYYY-MM-DD" replaced) to set a
#'    different date.
#' @param date_print Character string, defines the date format used in generated PDF documents.
#' @param gt_suffix Character string, suggests a file name so save to grant table in CSV format.
#' @param ... Optional additional arguments will be kept as they are in the resulting list.
#' @return A named list with entries \code{title}, \code{name}, \code{date}, \code{data_dir}, \code{out_dir}, \code{subdir}, \code{corr}, 
#'    \code{date}, \code{date_ISO}, \code{date_print}, \code{rename}, \code{dummies}, \code{marks}, and \code{gt_file}.
#' @export

klausur_meta <- function(
  title,
  name,
  data_dir,
  corr,
  marks,
  ignore,
  subdir_prefix,
  rename=c(),
  dummies=c(),
  disc.misc=FALSE,
  na.rm=TRUE,
  item.prefix=c(),
  out_dir=data_dir,
  date=Sys.Date(),
  date_print="%d.%m.%Y",
  gt_suffix="all.csv",
  ...
){
  date_ISO <- format(date, "%F")
  if(!missing(subdir_prefix)){
    subdir <- paste0(subdir_prefix, "_", date_ISO)
    data_dir <- file.path(data_dir, subdir)
    out_dir <- file.path(out_dir, subdir)
  } else {
    subdir <- NULL
  }
  # check if we need to generate mark assignments first
  if (length(names(marks)) > 0 & is.numeric(marks)){
    marks <- unlist(sapply(
      seq_along(marks), function(x){
        if(x > 1){
          rep(names(marks)[x], marks[x] - marks[x - 1])
        } else {
          rep(names(marks)[x], marks[x])}
        }
    ))
  } else {}
  result <- list(
    title=title,
    name=name,
    date=date,
    data_dir=data_dir,
    out_dir=out_dir,
    subdir=subdir,
    corr=corr,
    date=date,
    date_ISO=date_ISO,
    date_print=format(date, date_print),
    rename=rename,
    dummies=dummies,
    disc.misc=disc.misc,
    na.rm=na.rm,
    item.prefix=item.prefix,
    ...
  )

  if(!missing(marks)){
    result[["marks"]] <- marks
  } else {}
  if(!missing(ignore)){
    result[["ignore"]] <- ignore
  } else {}
  if(!missing(subdir_prefix)){
    result[["gt_file"]] <- file.path(out_dir, paste0(subdir_prefix, "_", date_ISO, "_", gt_suffix))
  } else {
    result[["gt_file"]] <- file.path(out_dir, paste0(date_ISO, "_", gt_suffix))
  }

  return(result)
}
