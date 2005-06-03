changequote([, ])

define([mode_all], [divert(0)])

define([site_title], [TYPHON solver site])
define([home_dir], [./$1])
define([img_dir], [img/[$1]])

define([skip_line], [<br><br>])
define([skip_row],  [<tr><td>&nbsp;</td></tr>])

define([set_page_title], [define([page_title], $1)])
define([set_site_title], [define([site_title], $1)])

define([html_header], [
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html" charset="UTF-8">
<link rel="stylesheet" href="styles.css" type="text/css">
<title>page_title - site_title</title>
</head>
<body text="#000000" link="#000080" vlink="#000080" alink="#800000"
background="img_dir([bgray.gif])">
])

define([html_footer], [
</body>
</html>
])

define([include_header], [
html_header
include([header$1.m4])
])

define([include_footer], [
include([footer$1.m4])
html_footer
])

define([margins], [
<table border=0 width="100%" cellpadding=0 cellspacing=0>
<tr valign="top">
<td width="[$1]%"></td>
<td align="justify">$3</td>
<td width="[$2]%"></td>
</tr></table>
])

define([include_image], [<img border=0 alt="[$1]" src="img_dir([$2])" [$3]>])

define([item], [include_image([-], [puce-tri.gif]) $1<br>])

define([item3d], [include_image([-], [tri-3d-blue.gif]) $1<br>])

define([hyperlink], [<a href="$2">$1</a>])

define([imglink], [hyperlink([include_image([$1], [$2], [$4])], [$3])])

define([section], [
  <table width=100% border=0 cellspacing=0 cellpadding=1 bgcolor=004b9b><tr><td>
  <table width=100% border=0 cellspacing=0 cellpadding=2 bgcolor=d7e6ef><tr><td>
  <b><span class='defaut'>[$1]</span></b>
  </td></tr></table> </td></tr></table>
])

define([sectionlr], [
  <table width=100% border=0 cellspacing=0 cellpadding=1 bgcolor=004b9b><tr><td>
  <table width=100% border=0 cellspacing=0 cellpadding=2 bgcolor=d7e6ef><tr class="defaut">
  <td><b>[$1]</b></td><td align=right>[$2]</td></tr></table> </td></tr></table>
])

define([google], [
<!-- Search Google -->
<FORM method=GET action="http://www.[google].com/search">
<TABLE><tr><td>
<INPUT TYPE=text name=q size=25 maxlength=255 value="">
<INPUT type=submit name=btnG VALUE="Google">
</td></tr></TABLE>
</FORM>
<!-- Search Google -->
])

define([normalbox],
[<table border=0 cellpadding=0 cellspacing=0 width="100%" bgcolor=0><tr><td>
<table border=0 cellpadding=4 cellspacing=1 width="100%"><tr>
<td align="left" bgcolor=DDDDCC>
<b>$1</b>
</td></tr><tr width="100%">
<td bgcolor=EEEEEE>$2</td>
</tr></table>
</td></tr></table>
]
)

dnl Tableau 2 colonnes pour placer des commentaires à gauche, et des 
dnl liens-images à droite.
define([linkbox], [
<table border=0 width="90%" cellspacing=0 cellpadding=1>
<tr valign="top">
<td align="left">$1</td>
<td align="center">$2</td>
</tr></table>
])

