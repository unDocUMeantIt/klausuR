# Copyright 2023-2024 Meik Michalke <meik.michalke@hhu.de>
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
#' @param data_file File name of the test raw data. Use a path releative to \code{data_dir}. If this argument
#'    is defined,  \code{\link[klausuR:klausur.data]{klausur.data}} will try to import this file using
#'    \code{\link[rio:import]{rio::import}}.
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
#'    Use something like \code{strptime("YYYY-MM-DD", format="\%Y-\%m-\%d")} (with "YYYY-MM-DD" replaced) to set a
#'    different date.
#' @param date_print Character string, defines the date format used in generated PDF documents.
#' @param gt_suffix Character string, suggests a file name for saving the \code{\link[klausuR:grand.table]{grand.table}} in a format supported by \code{\link[rio:export]{rio::export}}.
#' @param lang Character string, language to set internal defaults for \code{labels} (see below). Set to "de" for reports in German, English is the default.
#' @param labels Optional list of labels to customize the defaults used by other functions, see \code{\link[klausuR:report]{report}} for details on supported labels.
#' @param create_out_dir Logical, if \code{TRUE} and \code{out_dir} is missing, it will automatically be created.
#' @param ... Optional additional arguments will be kept as-is in the resulting list.
#' @return A named list with entries \code{title}, \code{name}, \code{date}, \code{data_dir}, \code{out_dir}, \code{subdir}, \code{corr}, 
#'    \code{date}, \code{date_ISO}, \code{date_print}, \code{rename}, \code{dummies}, \code{marks}, and \code{gt_file}.
#' @export

klausur_meta <- function(
    title
  , name
  , data_dir
  , data_file
  , corr
  , marks
  , ignore
  , subdir_prefix
  , rename = c()
  , dummies = c()
  , disc.misc = FALSE
  , na.rm = TRUE
  , item.prefix = c()
  , out_dir = data_dir
  , date = Sys.Date()
  , date_print = "%d.%m.%Y"
  , gt_suffix = "all.xlsx"
  , lang = "en"
  , labels = list()
  , create_out_dir = FALSE
  , ...
){
  date_ISO <- format(date, "%F")
  if(!missing(subdir_prefix)){
    subdir <- paste0(subdir_prefix, "_", date_ISO)
    data_dir <- file.path(data_dir, subdir)
    out_dir <- file.path(out_dir, subdir)
  } else {
    subdir <- NULL
  }
  if(missing(data_file)){
    data_file <- NULL
  } else {
    data_file_path <- file.path(data_dir, data_file)
    if(!file.exists(data_file_path)){
      stop(simpleError(paste0("The data file specified cannot be found:\n  ", data_file_path)))
    } else {}
  }

  # check if we need to generate mark assignments first
  marks <- convert_mark_assignments(marks=marks)

  result <- list(
      title = title
    , name = name
    , date = date
    , data_dir = data_dir
    , data_file = data_file
    , out_dir = out_dir
    , subdir = subdir
    , corr = corr
    , date = date
    , date_ISO = date_ISO
    , date_print = format(date, date_print)
    , rename = rename
    , dummies = dummies
    , disc.misc = disc.misc
    , na.rm = na.rm
    , item.prefix = item.prefix
    , lang = lang
    , labels = labels
    , ...
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

  if(isTRUE(create_out_dir) & !file_test("-d", file.path(result[["out_dir"]]))){
    message(paste0("Creating missing output directory:\n  ", result[["out_dir"]]))
    dir.create(file.path(result[["out_dir"]]), recursive=TRUE)
  } else {}

  return(result)
}
