dnl : t1 is supposed to define rank 1 menu 
dnl : t2 is supposed to define rank 2 submenu
dnl : id1 defines rank 1 page (undefined only if home)
dnl : id2 defines rank 2 page (undefined if page rank 0 or 1)
dnl : id3 defines rank 3 page (undefined if page rank 0, 1 or 2)

define([m_index],      [Home])
define([m_index_news],   [News])
define([m_index_log],    [Changes Log])
define([m_feat],       [Features])
define([m_feat_core],    [Core])
define([m_feat_ns],      [Navier-Stokes])
define([m_feat_kdif],    [Heat Transfer])
define([m_feat_input],   [Input])
define([m_feat_output],  [Output])
define([m_case],       [Examples])
define([m_dev],        [Development])
define([m_get],        [Download])

define([item_m2], [<img src=img_dir(tri-3d-blue.gif)>])

define([print_t1], [
  ifelse(id1, $1, 
    [<tr><td bgcolor=c7d6df>hyperlink(m_$1, $1.html)</td></tr>],
    [<tr><td>hyperlink(m_$1, $1.html)</td></tr>] )
])
define([print_t2], [
  ifelse(id2, $1, 
    [<td bgcolor=c7d6df>hyperlink([m_]id1[_]$1, id1[_]$1.html)</td>], 
    [<td>hyperlink([m_]id1[_]$1, id1[_]$1.html)</td>] )
])

dnl =========== write menu left ================================================

define([write_menu_left], [
<table width=100  border=0 cellspacing=4 cellpadding=2 class="menu1">
print_t1([index])
print_t1([feat])
print_t1([case])
print_t1([dev])
print_t1([get])
skip_row
<tr><td><A href="http://sourceforge.net"> <IMG src="http://sourceforge.net/sflogo.php?group_id=132321&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net" /></A></td></tr>
</table>
])

dnl =========== write menu head ================================================

define([write_menu_head], [
  <table width="100%" bgcolor="d7e6ef" border=0 cellspacing="0" cellpadding="4" class=menu1>
  dnl<table width="100%" background=img_dir([bg-menu-head.png]) border=0 cellspacing="0" cellpadding="4">
  <tr valign="middle">
  <td class=menu1></td>
  ifelse(id1, [index], [
    print_t2([news])
    print_t2([log]) ])
  ifelse(id1, [feat], [
    print_t2([core])
    print_t2([ns])
    print_t2([kdif])
    print_t2([input])
    print_t2([output]) ])
  <td class=menu1></td>
  </tr>
  </table>
])
