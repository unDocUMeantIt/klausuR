## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# Class klausuR.mult
#
# This class is used for objects that are returned by \code{\link[klausuR:klausur.mufo]{klausur.mufo}}.
#
# @title S4 class klausuR.mult
# @slot forms A vector with the names of all test forms.
# @slot results.part A list with the partial results of each test form
# @slot results.glob An object of class klausuR-class with overall results
# @name klausuR.mult,-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @exportClass klausuR.mult
# @rdname klausuR.mult-class

setClass("klausuR.mult",
  representation=representation(
      forms="vector",
      results.part="list",
      results.glob="klausuR"
  ),
  prototype(
      forms=NULL,
      results.part=list(),
      results.glob=new("klausuR")
  )
)

#setValidity("klausuR.mult", function(object){
#  
#})
