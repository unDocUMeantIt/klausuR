# internal package description
pck.descr.v 	<- "A set of functions designed to quickly generate results of
						a multiple choice test. Generates detailed global results, lists
						for anonymous feedback and personalised result feedback (in LaTeX
						and/or PDF format), as well as item statistics like Cronbach's alpha or
						disciminatory power."

pckg.dscrptn <- data.frame(
		Package="klausuR",
		Type="Package",
		Title="Multiple Choice Test Evaluation",
		Author="m.eik michalke",
		Maintainer="m.eik michalke <meik.michalke@hhu.de>",
		Depends="R (>= 2.9.0),xtable,psychometric,methods,graphics,tools",
		Enhances="rkward",
		Description=pck.descr.v,
		License="GPL (>= 3)",
		Encoding="UTF-8",
		LazyLoad="yes",
		URL="http://r-forge.r-project.org/projects/klausur",
		stringsAsFactors=FALSE)
