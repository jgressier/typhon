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
    var idloop = 'subitem_'+subitemnames[i];
    var dloop = document.getElementById(idloop);
    if (dloop) { dloop.style.display='none'; }
  }
  if (d) {d.style.display='block';}
}

