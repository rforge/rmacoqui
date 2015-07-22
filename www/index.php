
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
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

<p>
The method was first published by M&aacute;rquez et al. (1997), based on a prior proposal for <b>detecting biogeographic boundaries</b> (Real et al., 1992), and with a mathematic rationale derived from McCoy et al. (1986).
</p>

<p>
The method for <b>chorotype identification</b> was later enhanced by Mu&ntilde;oz et al. (2003) and Real et al. (2008), and was updated and contextualized under a <b>fuzzy logic framework</b> by Olivero et al. (2011).
</p>

<p>
<i>RMacoqui</i> outputs are also useful as the basis for delimiting <b>biogeographic regions</b> and <b>transition zones</b> with the support of fuzzy logic (Olivero et al. 2013).
</p>

<br />

<h2>
Installing, loading and using <i>RMacoqui</i>
</h2>

<p>
To <strong>install</strong> <i>RMacoqui</i> directly form R-Forge, paste the following command in the R console (while connected to the internet):
</p>

<code>
install.packages("RMacoqui", repos="http://R-Forge.R-project.org")
</code>
<br />

<p>
This should work if you have the <b>latest version of R</b>; otherwise, it may either fail (producing a message like "<i>package 'RMacoqui' is not available for your R version</i>") or install an older version of <i>RMacoqui</i> -- you can check the version that you have actually installed by typing <big><b><code>citation(package="RMacoqui")</code></b></big>. To make sure you install the latest <i>RMacoqui</i> version, you can either upgrade R <i>or</i> download the compressed <b>package source files</b> to your disk (<i>.zip</i> for Windows or <i>.tar.gz</i> for Linux and Mac, available <a href="http://r-forge.r-project.org/R/?group_id=2103">here</a>) and then install the package from the disk, e.g. with R menu "<i>Packages - Install packages from local zip files</i>" (Windows), or "<i>Packages & Data - Package installer, Packages repository - Local source package</i>" (Mac), or "<i>Tools - Install packages - Install from: Package Archive File</i>" (RStudio).</p>

<p>
Here's a <big><b><a href="RMacoqui-manual.pdf">reference manual</a></b></big> based on the package help files, and a very <big><b><a href="RMacoqui-tutorial.pdf">quick tutorial</a></b></big> of the package; a more detailed one is in preparation.
</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<br />

<h2> References </h2>

<p>
Baroni-Urbani C., Rufo S., Vigna-Taglianti A. (1978) Materiali per una biogeografia italiana fondata su alcuni generi di Coleotteri, Cicindelidi, Carabidi e Crisomelidi. <i>Estratto della Memorie della Societ&agrave; Entomologica Italiana</i> 56:35-92.
</p>

<p>
M&aacute;rquez A.L., Real R., Vargas J.M., Salvo A.E. (1997) On identifying common distribution patterns and their causal factors: a probabilistic method applied to pteridophytes in the Iberian Peninsula. <i>Journal of Biogeography</i> 24:613-631.
</p>

<p>
McCoy E.D., Bell S.S., Waters K. (1986) Identifying biotic boundaries along environmental gradients. <i>Ecology</i> 67:749-759.
</p>

<p>
Mu&ntilde;oz A.R., Real R., Olivero J., M&aacute;rquez A.L., Guerrero J.C., Barcena S.B., Vargas J.M. (2003) Biogeographical zonation of African hornbills and their biotic and geographic characterisations. <i>Ostrich</i> 74:39-47.
</p>

<strong>
<p>
Olivero J., Real R., M&aacute;rquez A.L. (2011) Fuzzy chorotypes as a conceptual tool to improve insight into biogeographic patterns. <i>Systematic Biology</i> 60:645-660.
</p>
</strong>

<p>
Olivero J., M&aacute;rquez A.L., Real R. (2013) Integrating fuzzy logic and statistics to improve the reliable delimitation of biogeographic regions and transition zones. <i>Systematic Biology</i> 62:1-21.
</p>

<p>
Real R., Olivero J., Vargas J.M. (2008) Using chorotypes to deconstruct biogeographical and biodiversity patterns: the case of breeding waterbirds in Europe. <i>Global Ecology and Biogeography</i> 17:735-746.
</p>

<p>
Real R., Vargas J.M., Guerrero J.C. (1992) An&aacute;lisis biogeogr&aacute;fico de clasificaci&oacute;n de &aacute;reas y especies. <i>Monograf&iacute;as de Herpetolog&iacute;a</i> 2:73-84.
</p>

</body>
</html>
