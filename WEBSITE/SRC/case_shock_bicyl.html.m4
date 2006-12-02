include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Examples of Shock Waves in Inviscid Flows])
define([id1], [case])
define([id2], [shock])
define([id3], [bicyl])
define([m_]id1[_]id2[_]id3, [BiCylinder])

include_javascript([showhide.js])
include_header

dnl -------------------------------------------------------------

item3d([m4_showitem([descript],  [Case description])])
item3d([m4_showitem([result],    [Results])])


dnl -------------------------------------------------------------
<span class="ghostitem" id="subitem_descript">
section([Case description])

twocols([
  include_image([bi-cylinder density], [comput/hypers-bicyl-mesh-small.png], [width=400])
], [
  <center><b>Two staggered cylinders in a Mach 10 hypersonic flow</b></center><br>
  item([Mesh: 2D hybrid quads and tri])
  item([Model: inviscid flow])
  item([Numerics: first order HLLC])
])
</span>

dnl -------------------------------------------------------------

<span class="ghostitem" id="subitem_result">
section([Results])
twocols([
  item([Density contours and mesh: two shock waves and interaction])
], [
  include_image([bi-cylinder density], [comput/hypers-bicyl-density-small.png],[width=400])
])
<br>
twocols([
  item([Streamlines colored by pressure:])<br> 
  streamlines are severely deviated through the shock waves
], [
  include_image([bi-cylinder density], [comput/hypers-bicyl-streamlines-pressure.png], [width=400])
])

</span>
skip_line


dnl -------------------------------------------------------------
include_footer
