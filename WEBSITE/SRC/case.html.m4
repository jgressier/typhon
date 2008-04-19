include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Examples])
define([id1], [case])
define([id2], [])
define([id3], [])

define([add_thumbnail], 
       [<a href=# onmouseover="show_main('$1', '$2')"><img border=1 height=150px src="img/comput/$1"/></a>])

include_header

[
<script type="text/javascript">
<!--
function show_main(image,title) {
  if (document.getElementById('main_txt')) { 
    document.getElementById('main_txt').innerHTML=title; 
  }
  if (document.getElementById('main_img')) { 
    document.getElementById('main_img').src="img/comput/"+image; 
    document.getElementById('main_img').style.display='block'; 
  }
}
//-->
</script>
]

include_javascript([showhide.js])

dnl -------------------------------------------------------------

section([List of Themes])

item([m4_showitem([2D],[2D examples])])
item([m4_showitem([Inviscid],[Inviscid examples])])
item([m4_showitem([Shock-Waves],[Shock-Waves examples])])
item([m4_showitem([Supersonic],[Supersonic examples])])


skip_line

dnl -------------------------------------------------------------

<span class="ghostitem" id="subitem_2D">
section([2D examples])
item([hyperlink([Hypersonic-BiCylinders], [example_Hypersonic-BiCylinders.html]): Hypersonic flow around two cylinders ])
item([hyperlink([Diamond], [example_Diamond.html]): Shock diffraction over a Pyramid ])
<BR>
</span>
<span class="ghostitem" id="subitem_Inviscid">
section([Inviscid examples])
item([hyperlink([Hypersonic-BiCylinders], [example_Hypersonic-BiCylinders.html]): Hypersonic flow around two cylinders ])
<BR>
</span>
<span class="ghostitem" id="subitem_Shock-Waves">
section([Shock-Waves examples])
item([hyperlink([Diamond], [example_Diamond.html]): Shock diffraction over a Pyramid ])
<BR>
</span>
<span class="ghostitem" id="subitem_Supersonic">
section([Supersonic examples])
item([hyperlink([Hypersonic-BiCylinders], [example_Hypersonic-BiCylinders.html]): Hypersonic flow around two cylinders ])
<BR>
</span>





skip_line


dnl -------------------------------------------------------------
include_footer
