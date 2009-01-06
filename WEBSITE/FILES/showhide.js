/*

*/


var nsubitem     = 0;
var subitemnames = new Array();

function js_additem(strname, title) {
  subitemnames[nsubitem] = strname;
  nsubitem++;
  document.write('<a onclick="javascript:js_showitem('+"'subitem_"+strname+"'"+');" href="#'+strname+'">'+title+'</a>');
}

function js_showitem(id) {
  var d = document.getElementById(id);
  for (var i = 0; i<nsubitem; i++) {
    var id = 'subitem_'+subitemnames[i];
    var d = document.getElementById(id);
    if (d) { d.style.display='none'; }
  }
  if (d) {d.style.display='block';}
}

