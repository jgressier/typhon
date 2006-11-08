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

dnl -------------------------------------------------------------

section([Examples and application news])

item([11/2005 : new two-cylinder case in inviscid hypersonic flow 
                (hyperlink([shock-waves], [case_shock.html]) [section])])

skip_line

dnl -------------------------------------------------------------

section([List of cases])

<div align=center>

add_thumbnail([hypers-bicyl-density-small.png], [2 cylinders in hypersonic flow])

skip_line

<div id=main_txt></div>
<img border=1 width=80% id=main_img style="display:none"/>
<br>
</div>


skip_line


dnl -------------------------------------------------------------
include_footer
