\name{klausur.data}
\alias{klausur.data}
\title{A function to create data objects with given and correct answers to a test.}
\usage{klausur.data(answ, corr, items, marks, wght, corr.key, rename=c(), na.rm=TRUE)
}
\description{A function to create data objects with given and correct answers to a test.}
\value{An object of class \code{\link[klausuR]{klausuR.answ-class}}.}
\arguments{\item{answ}{A \code{\link{data.frame}} which has to include at least these variables:
\code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, as well as \code{Pseudonym} (optional)
and variables for the answered items (according to the scheme \code{Item###},
where ### is a number with leading zeros, if needed).}
\item{corr}{A vector with the correct answers to all items in \code{answ} (named also according to \code{Item###}).}
\item{items}{Indices of a subset of variables in \code{answ} to be taken as items.}
\item{marks}{A vector assigning marks to points achieved (see details). Leave \code{NULL} if not available.}
\item{wght}{A vector with weights for each item (named also according to \code{Item###}). Leave \code{NULL} if not available.}
\item{corr.key}{If test has several test forms: A data.frame or matrix indicating the positions of all items (columns) in all
forms (rows). Must have a column called \code{Form} (like \code{answ}), and the item columns must follow the explained name
scheme \code{Item###}. \code{NULL} if not needed.
param rename A named vector defining if variables in \code{answ} need to be renamed into the klausuR name scheme. Accepts elements
named \code{No}, \code{Name}, \code{FirstName}, \code{MatrNo}, \code{Pseudonym} or \code{Form}. The values of these elements
represent the variable names of the input data.}
\item{na.rm}{Logical, whether cases with NAs should be ignored in \code{answ}. Defaults to TRUE.}
}

