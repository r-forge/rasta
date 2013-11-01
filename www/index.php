
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
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title><?php echo $group_name; ?></title>
    <!--<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />-->
    <link rel="stylesheet" href="css/style.css">
</head>

<body>

<div id="container">

<div id="header">
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a>
</div>

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

<p> <strong> Data sets and rasta package for windows: </strong> </p>

<p> <ul>
<li> <strong>Lesson 1-2:</strong> shape file kenya <a href="http://rasta.r-forge.r-project.org/kenyashape.zip">kenyashape.zip</a> </li>
<li> <strong>Lesson 4:</strong> data set. More info here (Sytze) <a href="http://rasta.r-forge.r-project.org/kroonven.csv">kroonven.csv</a> </li>
<li> <strong> Rasta </strong>package for windows (0.4 version). <a href="http://rasta.r-forge.r-project.org/rasta_04.zip">rasta_04.zip</a> </li>
</ul> </p>

<p> Contact: Jan.Verbesselt'@'wur.nl. To install the most recent version directly within R type: <strong> install.packages("rasta", repos="http://R-Forge.R-project.org") </strong> </p>
<p> Course developped at Wageningen University </p> 
<img src="http://bfast.r-forge.r-project.org/Logo_Wageningen_University.jpg" height="43" width="250" align=MIDDLE alt="Wageningen University">
<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</div><!-- close container -->
</html>
