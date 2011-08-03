## definition of the test suite
suite <- new ("RKTestSuite", id="klausuR",
	libraries = c ("klausuR"),
        # initCalls are run *before* any tests. Use this to set up the environment
        initCalls = list (
                function () {
			## these are example data sets from the ltm package
			library ("klausuR") # load klausuR library
			# prepare the needed data objects
			data("antworten")

			# vector with correct answers:
			answers <- c(Item01=3, Item02=2, Item03=2, Item04=2, Item05=4,
			Item06=3, Item07=4, Item08=1, Item09=2, Item10=2, Item11=4,
			Item12=4, Item13=2, Item14=3, Item15=2, Item16=3, Item17=4,
			Item18=4, Item19=3, Item20=5, Item21=3, Item22=3, Item23=1,
			Item24=3, Item25=1, Item26=3, Item27=5, Item28=3, Item29=4,
			Item30=4, Item31=13, Item32=234)
			assign("klausuRtest.answers", answers, envir=globalenv())

			# vector with assignement of marks:
			notenschluessel <- c()
			# scheme of assignments: marks[points_from:to] <- mark
			notenschluessel[0:12]  <- 5.0
			notenschluessel[13:15] <- 4.0
			notenschluessel[16:18] <- 3.7
			notenschluessel[19:20] <- 3.3
			notenschluessel[21]    <- 3.0
			notenschluessel[22]    <- 2.7
			notenschluessel[23]    <- 2.3
			notenschluessel[24]    <- 2.0
			notenschluessel[25:26] <- 1.7
			notenschluessel[27:29] <- 1.3
			notenschluessel[30:32] <- 1.0
			assign("klausuRtest.marks", notenschluessel, envir=globalenv())
                }
        ## the tests
        ), tests = list (
                new ("RKTest", id="MC_test_valuation", call=function () {
                        rk.call.plugin ("rkward::klausuR", antworten.available="antworten", chk_anon.state="anon", chk_cronbach.state="cronbach", chk_distrib.state="distrib", chk_globres.state="globres", chk_itemanal.state="itemanal", chk_mark_sugg.state="", chk_marks_sum.state="marks.sum", chk_matn_all.state="all", chk_na_replace.state="", chk_partial.state="", chk_reports.state="", chk_save.state="", chk_weights.state="weight", noten.available="klausuRtest.marks", richtig.available="klausuRtest.answers", submit.mode="submit")
                })
        ),
        # like initCalls: run after all tests to clean up.
	postCalls = list (
		function(){
			rm(list=c("klausuRtest.marks", "klausuRtest.answers", "antworten"), envir=globalenv())
		}
	)
)
