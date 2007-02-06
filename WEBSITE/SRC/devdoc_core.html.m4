include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Developer Guide -  data structures])
define([id1], [devdoc])
define([id2], [core])
define([id3], [])

include_header
include_javascript([showhide.js])

dnl -------------------------------------------------------------

<OL>
  <LI> Zones and Cycles</LI>
  <LI> Patch-based Unstructured Grid (ustmesh)
    <OL>
      <LI>m4_showitem([ustmesh],      [data structure])</LI>
      <LI>m4_showitem([ustmeshsplit], [split and merge])</LI>
     </OL></LI>
</OL>

dnl -------------------------------------------------------------
<span class="ghostitem" id="subitem_ustmesh">
section([Patch based Unstructured Grid Structure (ustmesh)])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem" id="subitem_ustmeshplit>
section([ustmesh split and merge])

<p>[]<br>
</p>
</span>



skip_line


dnl -------------------------------------------------------------
include_footer
