#----------------------------------------------------------------------
# gui_plot_init.tcl                                  Jérémie Gressier
#                                                      Juillet 2004
# Initialisation de l'interface graphique pour TYMON / Partie PLOT
#----------------------------------------------------------------------
# 07/2004 : création
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# guib:plot : Initilisation du BLT:GRAPH
#----------------------------------------------------------------------
proc guib:plot { f } {
  global guicolor guivar

  # Division de la frame principale : PanedWindow (package BWidget)
  PanedWindow $f.panedwin -side top -width 10 -bg $guicolor(projetbg)
  set pf0 [$f.panedwin add -minsize 400 -weight 6]
  set pf1 [$f.panedwin add -minsize 100 -weight 1] 
  $pf0 configure -bg $guicolor(projetwg)
  $pf1 configure -bg $guicolor(projetwg)

  # frame gauche : plot
  blt::graph $pf0.graph  -title "test"
  pack configure $pf0.graph -fill both -expand y
  lappend guivar(wlist4font) $pf0.graph

  # frame droite : 
  set fd [frame $pf1.f]
  TitleFrame $fd.opt -text "options" \
    -bg $guicolor(projetwg) -fg $guicolor(projetfg)  \
    -font $guivar(font)
  TitleFrame $fd.var -text "variables" \
    -bg $guicolor(projetwg) -fg $guicolor(projetfg)  \
    -font $guivar(font)
  lappend guivar(wlist4font) $fd.opt $fd.var

  guib:options   plotid [$fd.opt getframe]
  guib:variables plotid [$fd.var getframe]

  # pack

  pack configure $fd.opt -side top -fill x    -padx 3 -pady 3 
  pack configure $fd.var -side top -fill both -padx 3 -pady 3 -expand 1
  pack configure $fd               -fill both                 -expand 1

  return $f.panedwin
 
} ;# fin proc guib:plot

#----------------------------------------------------------------------
# guib:options : Initilisation des options
#----------------------------------------------------------------------
proc guib:options { plot f } {
  global guicolor guivar

  radiobutton $f.it -text iteration \
                    -bg $guicolor(projetwg) -fg $guicolor(projetfg) -font $guivar(font) \
                    -variable plot(tab:opt) -value iteration
  radiobutton $f.cy -text cycle  \
                    -bg $guicolor(projetwg) -fg $guicolor(projetfg) -font $guivar(font) \
                    -variable plot(tab:opt) -value cycle
  pack configure $f.it $f.cy -side top -anchor w -padx 5

} ;# fin proc guib:options

#----------------------------------------------------------------------
# guib:var : Initilisation des varions
#----------------------------------------------------------------------
proc guib:variables { plot f } {
  global guicolor guivar gplot
  
  foreach { var } $gplot(variables) {
    checkbutton $f.$var -text $var -variable $gplot($plot:$var) \
                -onvalue show -offvalue hide -command "plot:toggle_var $plot $var"
    pack $f.$var -side top -anchor w -padx 5 }
  #pack $f.list  

} ;# fin proc guib:variables

