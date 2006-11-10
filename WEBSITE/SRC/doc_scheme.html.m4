include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Documentation - Numerical Schemes])
define([id1], [doc])
define([id2], [scheme])
define([id3], [])

include_header
include_javascript([showhide.js])

dnl -------------------------------------------------------------

<OL>
  <LI> Convection based schemes
    <OL>
      <LI><script type="text/javascript"> js_additem("hlle",  "HLLE"); </script></LI>
      <LI><script type="text/javascript"> js_additem("hllc",  "HLLC"); </script></LI>
      <LI><script type="text/javascript"> js_additem("ausmm", "AUSM-M"); </script></LI>
    </OL></LI>
  <LI> Diffusion based schemes</LI>
  <LI> High order interpolation</LI>
    <OL>
      <LI><script type="text/javascript"> js_additem("gradient", "Gradient computation"); </script></LI>
      <LI><script type="text/javascript"> js_additem("muscl",    "MUSCL methods"); </script></LI>
      <LI><script type="text/javascript"> js_additem("limiter",  "Limiters"); </script></LI>
    </OL></LI>
  <LI> Time integration</LI>
    <OL>
      <LI><script type="text/javascript"> js_additem("explicit",  "Explicit method"); </script></LI>
      <LI><script type="text/javascript"> js_additem("implicit",  "Implicit method"); </script></LI>
      <LI><script type="text/javascript"> js_additem("matrix",    "Matrix resolution"); </script></LI>
    </OL></LI>
</OL>

dnl -------------------------------------------------------------
<span class="ghostitem" id="subitem_hlle">
section([HLLE scheme])

<p>[HLLE scheme is a variant of the two-waves HLL family originally proposed by Harten, Lax and Van Leer (1986).
This variant is the positive version of Einfeldt (1988). It is a very robust scheme which behaves as 
<acronym title="Flux Vector Splitting">FVS</acronym> schemes, 
even if it is originally based on a Riemann solver.]<br>
item([to use in TYPHON: specify m4_param([SCHEME=HLLE]) in m4_param([BLOCK:SPAT_PARAM])])
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_hllc">
section([HLLC scheme])

<p>[HLLC scheme is an extension of basic 2 waves HLL schemes. Slightly different extensions have
been proposed by Toro and Batten. All of them provide the third wave resolution which allows this scheme
to be accurate in contact discontinuities and boundary layers configurations.]<br>
item([to use in TYPHON: specify m4_param([SCHEME=HLLC]) in m4_param([BLOCK:SPAT_PARAM])])
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_ausmm">
section([AUSM-M scheme])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_gradient">
section([Gradient computation])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_muscl">
section([MUSCL methods])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_limiter">
section([Limiters])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_explicit">
section([Explicit time integration])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_implicit">
section([Implicit time integration])

<p>[]<br>
</p>
</span>

dnl -------------------------------------------------------------
<span class="ghostitem"  id="subitem_matrix">
section([Matrix resolution methods])

<p>[]<br>
</p>
</span>



dnl -------------------------------------------------------------
include_footer
