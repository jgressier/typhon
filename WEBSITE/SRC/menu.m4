dnl : t1 is supposed to define rank 1 menu 
dnl : t2 is supposed to define rank 2 submenu
dnl : id1 defines rank 1 page (undefined only if home)
dnl : id2 defines rank 2 page (undefined if page rank 0 or 1)
dnl : id3 defines rank 3 page (undefined if page rank 0, 1 or 2)

define([m_index],        [Home])
define([m_index_pres],     [Presentation])
define([m_index_news],     [News])
define([m_feat],         [Features])
define([m_feat_core],      [Core])
define([m_feat_ns],        [Navier-Stokes])
define([m_feat_kdif],      [Heat Transfer])
define([m_feat_input],     [Input])
define([m_feat_output],    [Output])
define([m_case],         [Examples])
define([m_case_shock],     [Shock waves])
define([m_case_viscous],   [Viscous])
define([m_case_amr],       [AMR])
define([m_case_heat],      [Heat transfer])
define([m_doc],          [User Guide])
define([m_doc_input],      [Input])
define([m_doc_output],     [Output])
define([m_doc_mesh],       [Mesh])
define([m_doc_ns],         [NS model])
define([m_doc_kdif],       [Conduction model])
define([m_doc_scheme],     [Numerical schemes])
define([m_doc_coupling],   [Coupling])
define([m_doc_amr],        [AMR])
define([m_doc_mpi],        [Parallelization])
define([m_dev],          [Development])
define([m_dev_team],       [Team])
define([m_dev_chlog],      [Change Log])
define([m_dev_rmap],       [Roadmap])
define([m_dev_sf],         [Sourceforge])
define([m_devdoc],       [Dev. Guide])
define([m_devdoc_core],     [Core])
define([m_devdoc_zone],     [Zone])
define([m_devdoc_grid],     [Grid])
define([m_devdoc_ustmesh],  [Ustmesh])
define([m_get],          [Download])
define([m_get_download],   [Download])
define([m_get_install],    [Installation])
define([m_get_faq],        [FAQ])

define([item_m2], [<img src=img_dir(tri-3d-blue.gif)>])

define([print_t1], [
  ifelse(id1, $1, 
    [<tr><td class=menu1act>hyperlink(m_$1, $1.html)</td></tr>],
    [<tr><td>hyperlink(m_$1, $1.html)</td></tr>] )
])
define([print_t2], [
  ifelse(id2, $1, 
    [<td class=menu2act>hyperlink([m_]id1[_]$1, id1[_]$1.html)</td>], 
    [<td class=menu2>   hyperlink([m_]id1[_]$1, id1[_]$1.html)</td>] )
])

dnl =========== write menu left ================================================

define([write_menu_left], [
<table width=110  border=0 cellpadding=4 cellspacing=0 class=menu1>
skip_row
print_t1([index])
print_t1([feat])
print_t1([case])
print_t1([doc])
print_t1([dev])
print_t1([devdoc])
print_t1([get])
skip_row
<tr><td><A href="http://sourceforge.net"> <IMG src="http://sourceforge.net/sflogo.php?group_id=132321&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net" /></A></td></tr>
skip_row
</table>
])

dnl =========== write menu head ================================================

define([write_menu_head], [
  <table width="100%" height=25 bgcolor="d7e6ef" background="img_dir([hbar-bluegrad.png])" border=0 cellspacing="0" cellpadding="0" class=menu2>
  <tr>
  <td> imglink([back], [back-arrow.png], id1.html)</td>
  <td align=center>
  <table border=0 cellspacing="0" cellpadding="4" height=25 class=menu2>
  <tr valign="middle">
  ifelse(id1, [index], [
    print_t2([pres])
    print_t2([news])
    ])
  ifelse(id1, [feat], [
    print_t2([core])
    print_t2([ns])
    print_t2([kdif])
    print_t2([input])
    print_t2([output])
    ])
  ifelse(id1, [case], [
    print_t2([shock])
    print_t2([viscous])
    print_t2([amr])
    print_t2([heat])
    ])
  ifelse(id1, [doc], [
    print_t2([input])
    print_t2([output])
    print_t2([mesh])
    print_t2([ns])
    print_t2([kdif])
    print_t2([scheme])
    print_t2([coupling])
    print_t2([amr])
    print_t2([mpi])
    ])
  ifelse(id1, [dev], [
    print_t2([team])
    print_t2([chlog])
    print_t2([rmap])
    print_t2([sf]) 
    ])
  ifelse(id1, [devdoc], [
    print_t2([core])
    print_t2([zone])
    print_t2([grid])
    print_t2([ustmesh]) 
    ])
  ifelse(id1, [get], [
    print_t2([download])
    print_t2([install])
    print_t2([faq]) ])
  <td width="1*" class=menu1></td>
  </tr>
  </table>

  </td></tr>
  </table>
])
