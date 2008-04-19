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

!!THEME-LIST!!

skip_line

dnl -------------------------------------------------------------

!!INSERT-EXAMPLES!!




skip_line


dnl -------------------------------------------------------------
include_footer
