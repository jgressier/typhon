include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Documentation - Numerical Schemes])
define([id1], [doc])
define([id2], [scheme])
define([id3], [])

include_header
[
<script type="text/javascript">
<!--
var nsubitem     = 0;
var subitemnames = new Array();

function additem(strname, title) {
  subitemnames[nsubitem] = strname;
  nsubitem++;
  document.write('<a onclick="javascript:showitem('+"'subitem_"+strname+"'"+');" href="#">'+title+'</a>');
}

function showitem(id) {
  var d = document.getElementById(id);
  for (var i = 0; i<nsubitem; i++) {
    var id = 'subitem_'+subitemnames[i];
    if (document.getElementById(id)) { document.getElementById(id).style.display='none'; }
  }
  if (d) {d.style.display='block';}
}
//-->
</script>
]
dnl -------------------------------------------------------------

<OL>
  <LI> Convection based schemes
    <OL>
      <LI><script type="text/javascript"> additem("hlle",  "HLLE"); </script></LI>
      <LI><script type="text/javascript"> additem("hllc",  "HLLC"); </script></LI>
      <LI><script type="text/javascript"> additem("ausmm", "AUSM-M"); </script></LI>
    </OL></LI>
  <LI> Diffusion based schemes</LI>
  <LI> High order interpolation</LI>
  <LI> Time integration</LI>
</OL>

<span class="ghostitem" id="subitem_hlle">
section([HLLE scheme])
HLLE scheme is a variant of the two-waves HLL family originally proposed by Harten, Lax and Van Leer (1986). This variant is
the positive version of Einfeldt (1988). It is a very robust scheme which behaves as 
<acronym title="Flux Vector Splitting">FVS</acronym> schemes, even if it is originally based on a Riemann solver.
skip_line
</span>

<span class="ghostitem"  id="subitem_hllc">
section([HLLC scheme])
skip_line
</span>

<span class="ghostitem"  id="subitem_ausmm">
section([AUSM-M scheme])
skip_line
</span>

skip_line


dnl -------------------------------------------------------------
include_footer
