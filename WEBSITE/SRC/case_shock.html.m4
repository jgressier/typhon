include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Examples of Shock Waves in Inviscid Flows])
define([id1], [case])
define([id2], [shock])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Steady supersonic or hypersonic flows])
<p>
  item3d([hyperlink([Two staggered cylinders interaction], [case_shock_bicyl.html])])
</p>

section([Unsteady shock-wave interactions])
<p>
  item3d([Backward step diffraction])
  item3d([Forward step reflexion (Emery case)])
  item3d([Pyramid reflexion-diffraction])
</p>


skip_line


dnl -------------------------------------------------------------
include_footer
