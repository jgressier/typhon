dnl : t1 is supposed to define rank 1 menu 
dnl : t2 is supposed to define rank 2 submenu
dnl : id1 defines rank 1 page (undefined only if home)
dnl : id2 defines rank 2 page (undefined if page rank 0 or 1)
dnl : id3 defines rank 3 page (undefined if page rank 0, 1 or 2)

define([m1_index], [Home])
define([m1_feat],  [Features])
define([m1_case],  [Examples])
define([m1_dev],   [Development])
define([m1_get],   [Download])

define([item_m2], [<img src=img_dir(tri-3d-blue.gif)>])

define([print_t1], [
  <tr><td class=menu1>hyperlink(m1_$1, $1.html)</td></tr>
])

dnl =========== write menu left ================================================

define([write_menu_left], [
<table width=100  border=0 cellspacing=1 cellpadding=4 class="menu1">
print_t1([index])
print_t1([feat])
print_t1([case])
print_t1([dev])
print_t1([get])
<tr><td><A href="http://sourceforge.net"> <IMG src="http://sourceforge.net/sflogo.php?group_id=132321&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net" /></A></td></tr>
</table>
])

dnl =========== write menu head ================================================

define([write_menu_head], [
  <table width="100%" bgcolor="d7e6ef" border=0 cellspacing="0" cellpadding="4">
  dnl<table width="100%" background=img_dir([bg-menu-head.png]) border=0 cellspacing="0" cellpadding="4">
  <tr valign="middle">
  <td class=menu1></td>
  <td class=menu1>test1</td>
  <td class=menu1>test2</td>
  <td class=menu1></td>
  </tr>
  </table>
])
