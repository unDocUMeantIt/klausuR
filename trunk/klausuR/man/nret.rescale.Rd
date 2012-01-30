\name{nret.rescale}
\alias{nret.rescale}
\title{Rescale test evaluation results}
\usage{
  nret.rescale(res.obj, score = "NRET", points = TRUE,
    percent = TRUE, marks = TRUE)
}
\arguments{
  \item{res.obj}{An object of class \code{klausuR} with
  results of an ET/NRET coded test.}

  \item{score}{Either \code{"NR"}, \code{"ET"},
  \code{"NRET"} or \code{"NRET+"}, defining the scoring
  function used.}

  \item{points}{Logical, whether point values should be
  rescaled.}

  \item{percent}{Logical, whether the percentage of
  received points should be rescaled, so that 0 points are
  0 percent.}

  \item{marks}{Logical, whether the assigned marks should
  be rescaled to fit the rescaled points (probably a good
  idea).  However, this will removed the attached mark
  assignment vecor, since its indices can't be negative.}
}
\value{
  An object of class \code{klausuR} with rescaled results.
}
\description{
  By default \code{klausur} adds a constant to results of
  ET/NRET type tests. This ensures that the minimum value
  of points can't fall below zero. If you'd rather like to
  see the results without this constant, i.e. results can
  be negative, you can rescale them with this function.
}
\author{
  m.eik michalke \email{meik.michalke@uni-duesseldorf.de}
}
\keyword{misc}

