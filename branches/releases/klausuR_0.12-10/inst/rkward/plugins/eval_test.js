// global vars
var chk_mufo;
var cronbach;
var itemanal;
var chk_matn_all; // all
var rep_matn;

function preprocess () {
	// we'll need some functions...
	echo("require(klausuR)\n\n");
}

function calculate () {
	// let's read all values into php variables for the sake of readable code
	var wght               = getValue("wght");
	var chk_partial        = getValue("chk_partial");
	var drp_partial        = getValue("drp_partial");
	var drp_mark_sugg      = getValue("drp_mark_sugg");
	var drp_mark_schm      = getValue("drp_mark_schm");
	var noten              = getValue("noten");
	var chk_na_remove      = getValue("chk_na_remove");
	// all tests or one test subject?
	chk_matn_all           = getValue("chk_matn_all"); // all
	rep_matn               = getValue("rep_matn");
	// multiple test forms?
	chk_mufo               = getValue("chk_mufo");
	// toggle cronbach's alpha and item analysis
	cronbach               = getValue("chk_cronbach");
	itemanal               = getValue("chk_itemanal");

	// individual reports
	var chk_reports        = getValue("chk_reports"); // TRUE
	var report_path        = getValue("report_path");
	var chk_rep_anonym     = getValue("chk_rep_anonym");
	var rep_anonym_file    = getValue("rep_anonym_file");
	var chk_rep_global     = getValue("chk_rep_global");
	var rep_global_file    = getValue("rep_global_file");
	var drp_report_lang    = getValue("drp_report_lang");
	var drp_filenames      = getValue("drp_filenames");
	var report_title       = getValue("report_title");
	var report_name        = getValue("report_name");
	var report_date        = getValue("report_date");
	var chk_hist_points    = getValue("chk_hist_points"); // TRUE
	var chk_hist_marks     = getValue("chk_hist_marks"); // TRUE
	var chk_marks_info_pts = getValue("chk_marks_info_pts"); // FALSE
	var chk_marks_info_pct = getValue("chk_marks_info_pct"); // FALSE
	var drp_format         = getValue("drp_format"); // pdf
	var chk_alt_comma      = getValue("chk_alt_comma"); // split answer alternatives

	// combine the description
	var rep_description = new Array();
		if(report_title)
			rep_description[rep_description.length] = "title=\""+report_title+"\"" ;
		if(report_name)
			rep_description[rep_description.length] = "name=\""+report_name+"\"" ;
		if(report_date)
			rep_description[rep_description.length] = "date=\""+report_date+"\"" ;

		// check for parallel testing
		if(chk_mufo){
			echo("klsr.mufo.obj <- klausur.mufo(data="+getValue("antworten"));
		} else {
			echo("klsr.obj <- klausur(data="+getValue("antworten"));
		}

		// any additional options?
		if(drp_mark_sugg == "individual" && noten) echo(", marks="+noten);
			if(drp_mark_sugg == "suggest") echo(", marks=\"suggest\"");
			if(drp_mark_sugg == "suggest" && drp_mark_schm) echo(", mark.labels=\""+drp_mark_schm+"\"");
		if(wght) echo(", wght="+wght);
		if(chk_partial) echo(", score=\""+drp_partial+"\"");
		if(!chk_na_remove) echo(", na.rm=FALSE");
		if(!cronbach) echo(", cronbach=FALSE");
		if(!itemanal) echo(", item.analysis=FALSE");
		echo(")\n");
		if(chk_mufo){
			echo("# For convenience extract global results\nklsr.obj <- klsr.mufo.obj@results.glob\n\n");
		} else {}

		// should individual reports be written to disk?
		if(chk_reports){
		echo("## Write reports\n"+
		"klausur.report(klsr=klsr.obj");
			if(chk_matn_all) echo(", matn=\"all\"");
				else if(rep_matn) echo(", matn="+rep_matn);
			if(drp_format == "pdfmerge") echo(", pdf=TRUE, merge=TRUE");
				else if(drp_format == "pdf") echo(", pdf=TRUE");
				else if(drp_format == "pdfmergelatex") echo(", save=TRUE, pdf=TRUE, merge=TRUE");
				else if(drp_format == "pdflatex") echo(", save=TRUE, pdf=TRUE");
				else echo(", save=TRUE");
			if(report_path) echo(", path=\""+report_path+"\"");
			if(drp_filenames != "matn") echo(", file.name=\""+drp_filenames+"\"");
			if(chk_hist_points == "TRUE" || chk_hist_marks == "TRUE") echo(", hist=list(points="+chk_hist_points+", marks="+chk_hist_marks+")");
			if(chk_marks_info_pts == "TRUE" || chk_marks_info_pct == "TRUE") echo(", marks.info=list(points="+chk_marks_info_pts+", percent="+chk_marks_info_pct+")");
			if(rep_description) echo(", descr=list("+rep_description.join(", ")+")");
			if(drp_report_lang != "en") echo(", lang=\""+drp_report_lang+"\"");
			if(chk_alt_comma != "TRUE") echo(", alt.candy=FALSE");
		echo(", quiet=TRUE)\n\n");

		// should anonymous feedback be generated?
		if(chk_rep_anonym){
			echo("## Write anonymous feedback report\n"+
			"klausur.report(klsr=klsr.obj, matn=\"anon\"");
			if(drp_format == "latex" || drp_format == "pdfmergelatex" || drp_format == "pdflatex") echo(", save=TRUE");
			if(drp_format == "pdf" || drp_format == "pdfmergelatex" || drp_format == "pdflatex" || drp_format == "pdfmerge") echo(", pdf=TRUE");
			if(report_path) echo(", path=\""+report_path+"\"");
			if(chk_marks_info_pts == "TRUE" || chk_marks_info_pct == "TRUE") echo(", marks.info=list(points="+chk_marks_info_pts+", percent="+chk_marks_info_pct+")");
			if(rep_description) echo(", descr=list("+rep_description.join(", ")+")");
			if(drp_report_lang != "en") echo(", lang=\""+drp_report_lang+"\"");
			if(rep_anonym_file != "anon.tex") echo(", anon.glob.file=\""+rep_anonym_file+"\"");
			echo(", quiet=TRUE)\n\n");
		}

		// should a global report be generated?
		if(chk_rep_global){
			echo("## Write global results report\n"+
			"klausur.report(klsr=klsr.obj, matn=\"glob\"");
			if(drp_format == "latex" || drp_format == "pdfmergelatex" || drp_format == "pdflatex") echo(", save=TRUE");
			if(drp_format == "pdf" || drp_format == "pdfmergelatex" || drp_format == "pdflatex" || drp_format == "pdfmerge") echo(", pdf=TRUE");
			if(report_path) echo(", path=\""+report_path+"\"");
			if(chk_marks_info_pts == "TRUE" || chk_marks_info_pct == "TRUE") echo(", marks.info=list(points="+chk_marks_info_pts+", percent="+chk_marks_info_pct+")");
			if(rep_description) echo(", descr=list("+rep_description.join(", ")+")");
			if(drp_report_lang != "en") echo(", lang=\""+drp_report_lang+"\"");
			if(rep_global_file) echo(", anon.glob.file=\""+rep_global_file+"\"");
			echo(", quiet=TRUE)\n\n");
		}
	}
}

function printout () {
	// printing options
	var globres        = getValue("chk_globres");
	var anon           = getValue("chk_anon");
	var marks_sum      = getValue("chk_marks_sum");
	var distrib        = getValue("chk_distrib");

	// save options
	var save           = getValue("save_results.active");
	var save_name      = getValue("save_results");
	var save_env       = getValue("save_results.parent");

	echo("rk.header(\"klausuR: Test Evaluation\")\n");

	// is this for *all* tests or *one* of the test subjects?
	if(chk_matn_all) {
		if(globres) {
			echo("rk.print(\"<h3>Global Results</h3>\")\n"+
					"rk.print(klsr.obj@results)\n");
		}
		if(anon) {
			echo("rk.print(\"<h3>Anonymous Feedback</h3>\")\n"+
					"rk.print(klsr.obj@anon)\n");
		}
		if(marks_sum) {
			echo("rk.print(\"<h3>Mark definitions (effectively)</h3>\")\n"+
					"rk.print(klsr.obj@marks.sum)\n");
		}
		if(distrib) {
			echo("rk.print(\"<h3>Mean</h3>\")\n"+
					"rk.print(klsr.obj@mean)\n"+
					"rk.print(\"<h3>Standard Deviation</h3>\")\n"+
					"rk.print(paste(\"sd: \",round(klsr.obj@sd, 2), sep=\"\"))\n");
		}
		if(cronbach) {
			echo("rk.print(\"<h3>Cronbach&apos;s alpha</h3>\")\n"+
					"if(!is.na(klsr.obj@cronbach$alpha)){\n"+
					"  cr.alpha <- paste(\"alpha: \", round(klsr.obj@cronbach$alpha, 2), \"<br />Confidence interval: \",\n"+
					"	      round(klsr.obj@cronbach$ci$LCL, 2),\"-\",\n"+
					"	      round(klsr.obj@cronbach$ci$UCL, 2),\" (95%)\",sep=\"\")}\n"+
					"else {cr.alpha <- \"Error: Cronbach's alpha is NA\"}\n"+
					"rk.print(cr.alpha)\n");
			if(!itemanal)
				echo("rk.print(klsr.obj@cronbach$deleted)\n");
		}
		if(itemanal) {
			echo("rk.print(\"<h3>Item Anlysis</h3>\")\n"+
					"if(length(klsr.obj@item.analysis) > 1 && !is.na(klsr.obj@item.analysis)){\n"+
					"  item.analysis <- data.frame(Diffc=klsr.obj@item.analysis$Difficulty,\n"+
					"	      DiscrPwr=klsr.obj@item.analysis$Item.total,\n"+
					"	      PartWhole=klsr.obj@item.analysis$Item.Tot.woi,\n"+
					"	      Discrim=klsr.obj@item.analysis$Discrimination");
			if(cronbach) {
				echo(",\n	      alphaIfDeleted=klsr.obj@item.analysis$alphaIfDeleted");
			}
			echo(")\n"+
					"  dimnames(item.analysis)[[1]] <- dimnames(klsr.obj@item.analysis)[[1]]\n"+
					"} else {\n"+
					"  item.analysis <- \"Error: Item analysis is NA\"\n}\n"+
					"rk.print(item.analysis)\n");
		}
	} // end if(all_tests)
	else {
		echo("rk.print(\"<h3>Individual results</h3>\")\n"+
				"rk.print(klsr.obj@results[klsr.obj@results$MatrNo == ");
			if(rep_matn) echo(rep_matn);
			echo(",])\n"+
				"items.solved <- klsr.obj@trfls[klsr.obj@results$MatrNo == ");
			if(rep_matn) echo(rep_matn);
			echo(",-1]\n"+
				"rownames(items.solved) <- \"Solved\"\n"+
				"rk.print(\"<h3>Items solved</h3>\")\n"+
				"rk.print(t(items.solved))\n");
	} // end else

	// check if results are to be saved:
	if(save) {
		if(chk_mufo){
			echo("assign(\""+save_name+"\", klsr.mufo.obj, envir="+save_env+")\n");
		} else {
			echo("assign(\""+save_name+"\", klsr.obj, envir="+save_env+")\n");
		}
	}
}
