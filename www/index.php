
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

  <h4>What can you do with klausuR?</h4>

	<p>This package is intended for persons or institutions who/which apply Multiple Choice (MC) tests. Work on it started off in 2009, as a collection of functions we used in our own department (University of D&uuml;sseldorf, Institute of Experimental Psychology, Department of Diagnostics and Differential Psychology). Its main purpose is to evaluate results for each test subject, assign marks accordingly, and produce printable summaries (e.g., to be archived with the test, to be able to verify the correctness of results any time).</p>

	<p>Over time the package gained a lot of features we don't actually needed ourselves, but which might be interesting for research. For instance it cannot only apply the typical Number Right scoring (one point for each correctly marked alternative), but also Coombs Elimination Testing and Number Right Elimination Testing (development version).</p>

	<h4>Core features</h4>
	<ul>
		<li>Global results (<a href="http://R.reaktanz.de/klausuR/klausuR-global.pdf" title="klausuR (global results)">example PDF</a>), including</li>
		<ul>
			<li>Achieved number of points</li>
			<li>Percentage of correct answers</li>
			<li>Marks, according to a given key</li>
		</ul>
		<li>Anonymous feedback (e.g., to publish the results; <a href="http://R.reaktanz.de/klausuR/klausuR-anonym.pdf" title="klausuR (anonymous feedback)">example PDF</a>)</li>
		<li>Detailed individual results (<a href="http://R.reaktanz.de/klausuR/klausuR-reports.pdf" title="klausuR (individual feedback)">example PDF</a>), including</li>
		<ul>
			<li>A table listing given answers, correct answers, and points gained for each test item</li>
			<li>Optionally as LaTeX report</li>
			<li>Direct export of PDF reports</li>
			<li>Optionally including distribution graphs of global results (points and/or marks)</li>
		</ul>
		<li>Test item analysis</li>
		<ul>
			<li>Cronbach's alpha</li>
			<li>Discriminatory power (part-whole corrected)</li>
			<li>Difficulty</li>
		</ul>
		<li>Optionally suggestions for mark assignments by distribution (e.g., according to DIHK standards)</li>
		<li>Various ways of coping with partial answers (incl. ET, NRET)</li>
		<li>Can cope with several parallel test forms</li>
		<li>Comparison of two sets manually entered results</li>
	</ul>

	<h4>Graphical user interface</h4>
	<p>There is also a GUI to work with the package, implemented as a <a href="http://reaktanz.de/?c=hacking&amp;s=klausuR#RKWard" title="RKWard-plugin">plugin for RKWard</a>.<p>

  <h4>Limitations</h4>

  <p>Some of the advanced features, especially in the development version, are not extensively tested in all possible combinations. If you should run into a bug, please don't hesitate to send a report! But even if everything runs smoothly, <strong>please do always check at least a sample of the results to make sure no errors occurred</strong>.</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
