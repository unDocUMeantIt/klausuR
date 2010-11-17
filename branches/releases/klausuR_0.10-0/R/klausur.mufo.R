klausur.mufo <- function(answ.mult, corr.mult, marks, mark.labels=NULL, items=NULL, wght=NULL, score="solved", matn=NULL, na.replace=NULL, cronbach=TRUE, item.analysis=TRUE){

    ## first we'll check if the data is sane
    # are we given any parallel forms at all?
    if(is.null(answ.mult[["Form"]]) || is.null(dim(corr.mult)) || is.null(corr.mult[["Form"]])){
      stop(simpleError("Both answ.mult and corr.mult must contain the variable \"Form\"!"))
    } else {}
    # ok, so how many test forms are there?
    test.forms.answ <- levels(as.factor(answ.mult[["Form"]]))
    test.forms.corr <- levels(as.factor(corr.mult[["Form"]]))
    # the relevant value is what's in answ.mult. it won't hurt if corr.mult has more,
    # as long as it holds all relevant data for the forms in answ.mult
    missing.forms <- !is.element(test.forms.answ, test.forms.corr)
    # if they are not compatible, we'll cry a little. and then stop.
    if(sum(missing.forms) > 0){
      stop(simpleError(paste("Sorry, but corr.mult lacks the correct answers to test form", paste(test.forms.answ(missing.forms), collapse=", "))))
    } else {}

    # if a user accidently(?) submits only one test form, it's clearly a case for klausur()
    # we'll just hand it over and return the results
    if(length(test.forms.answ) == 1){
      warning("Only one test form was supplied. Called klausur() instead.")
      klausur.mufo.results <- klausur(answ=answ.mult, corr=corr.mult[corr.mult$Form==test.forms.answ,], marks=marks,
				      mark.labels=mark.labels, items=items, wght=wght, score=score, matn=matn,
				      na.replace=na.replace, cronbach=cronbach, item.analysis=item.analysis)
      ## calculation would end here if only one form was submitted
    }
    else {
      ## separate forms and calculate partial results
      # we'll call klausur() for each test form, and later combine the results
      # daten nur einer form zurückgewinnen
      # daten.alles.ab[daten.alles.ab$Form=="a",]
      # richtig.ab[richtig.ab$Form=="a",]

      ## combine global results

    }

    return(klausur.mufo.results)
}