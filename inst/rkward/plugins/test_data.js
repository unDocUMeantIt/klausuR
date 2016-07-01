function preprocess () {
	// we'll need some functions...
	echo("require(klausuR)\n\n");
}

function calculate () {
	// let's read all values into php variables for the sake of readable code
	var richtig				= getValue("richtig");
	var noten				= getValue("noten");
	var chk_mufo			= getValue("chk_mufo");
	var mufo_corr_key		= getValue("mufo_corr_key");
	var chk_na_remove		= getValue("chk_na_remove");

	// advanced options
	var wght						= getValue("wght");

	var rename_no				= getValue("rename_no");
	var rename_name			= getValue("rename_name");
	var rename_firstname		= getValue("rename_firstname");
	var rename_matrno			= getValue("rename_matrno");
	var rename_pseudonym		= getValue("rename_pseudonym");
	// combine the rename values
	var rename_values = new Array();
		if(rename_no)
			rename_values[rename_values.length] = "No=\""+rename_no.replace(/(.*\[\[\")(.*)(\"\]\])/g, '$2')+"\"" ;
		if(rename_name)
			rename_values[rename_values.length] = "Name=\""+rename_name.replace(/(.*\[\[\")(.*)(\"\]\])/g, '$2')+"\"" ;
		if(rename_firstname)
			rename_values[rename_values.length] = "FirstName=\""+rename_firstname.replace(/(.*\[\[\")(.*)(\"\]\])/g, '$2')+"\"" ;
		if(rename_matrno)
			rename_values[rename_values.length] = "MatrNo=\""+rename_matrno.replace(/(.*\[\[\")(.*)(\"\]\])/g, '$2')+"\"" ;
		if(rename_pseudonym)
			rename_values[rename_values.length] = "Pseudonym=\""+rename_pseudonym.replace(/(.*\[\[\")(.*)(\"\]\])/g, '$2')+"\"" ;

	var chk_dummy_no				= getValue("chk_dummy_no");
	var chk_dummy_name			= getValue("chk_dummy_name");
	var chk_dummy_firstname		= getValue("chk_dummy_firstname");
	var chk_dummy_matrno			= getValue("chk_dummy_matrno");
	var chk_dummy_pseudonym		= getValue("chk_dummy_pseudonym");
	// combine the dummy values
	var dummy_values = new Array();
		if(chk_dummy_no)
			dummy_values[dummy_values.length] = "\"No\"" ;
		if(chk_dummy_name)
			dummy_values[dummy_values.length] = "\"Name\"" ;
		if(chk_dummy_firstname)
			dummy_values[dummy_values.length] = "\"FirstName\"" ;
		if(chk_dummy_matrno)
			dummy_values[dummy_values.length] = "\"MatrNo\"" ;
		if(chk_dummy_pseudonym)
			dummy_values[dummy_values.length] = "\"Pseudonym\"" ;

	var chk_disc_misc				= getValue("chk_disc_misc");

	echo("klsr.data.obj <- klausur.data(answ="+getValue("antworten"));
		// any additional options?
		if(richtig) echo(", corr="+richtig);
		if(noten) echo(", marks="+noten);
		if(wght) echo(", wght="+wght);
		if(chk_mufo && mufo_corr_key) echo(", corr.key="+mufo_corr_key);
		if(rename_values.length > 0) echo(", rename=c("+rename_values.join(", ")+")");
		if(dummy_values.length > 0) echo(", dummies=c("+dummy_values.join(", ")+")");
		if(chk_disc_misc) echo(", disc.misc=TRUE");
		if(!chk_na_remove) echo(", na.rm=FALSE");
		echo(")\n");
}

function printout () {
	var saveDataName		= getValue("saveKlsrData");
	var saveDataEnv		= getValue("saveKlsrData.parent");

	echo("rk.header(\"klausuR: Prepare Test Data\")\n");

	// is this for *all* tests or *one* of the test subjects?
// 	if(chk_matn_all) {
// 		if(globres) {
// 			echo("rk.print(\"<h3>Global Results</h3>\")\n"+
// 					"rk.print(klsr.obj@results)\n");
// 		}
// 		if(distrib) {
// 			echo("rk.print(\"<h3>Mean</h3>\")\n"+
// 					"rk.print(klsr.obj@mean)\n"+
// 					"rk.print(\"<h3>Standard Deviation</h3>\")\n"+
// 					"rk.print(paste(\"sd: \",round(klsr.obj@sd, 2), sep=\"\"))\n");
// 		}
// 	} // end if(all_tests)
// 	else {} // end else

	// save results:
 	echo("assign(\""+saveDataName+"\", klsr.data.obj, envir="+saveDataEnv+")\n");
}
