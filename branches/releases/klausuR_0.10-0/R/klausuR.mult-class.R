#' Class klausuR.mult
#'
#' This class is used for objects that are returned by \code{\link[klausuR:klausur.mufo]{klausur.mufo}}.
#'
#' @title S4 class klausuR.mult
#' @slot forms A vector with the names of all test forms.
#' @slot results.part A list with the partial results of each test form
#' @name klausuR.mult
#' @import methods
#' @keywords classes
#' @author m.eik michalke \email{meik.michalke@@uni-duesseldorf.de}
#' @exportClass klausuR.mult
#' @rdname klausuR.mult-class

setClass("klausuR.mult",
representation=representation(
    forms="vector",
    results.part="list"
#    results.glob="list",
#    answ="data.frame",
#    corr="vector",
#    marks="vector",
#    marks.sum="matrix",
#    trfls="data.frame",
#    anon="data.frame",
#    mean="table",
#    sd="numeric",
#    cronbach="list",
#    item.analysis="data.frame"
))

#setValidity("klausuR.mult", function(object){
#  
#})
