#----------------------------------------------------------------------
# tymon_settings.tcl                                  Jérémie Gressier
#                                                       décembre 2003
# Commandes TYMON pour la défintion des paramètres
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# settings:init_gui
#----------------------------------------------------------------------
proc settings:init_gui {} {
  global projet guivar guicolor

  set top [toplevel .settings]
  wm title $top "settings"
  set f1 [frame $top.f1 -relief sunken -borderwidth 2]
  set f2 [frame $top.f2]
  
  set b_ok [button $f2.bok -text Ok -command settings:Ok -relief raised \
      -bg $guicolor(mbuttbg) -fg $guicolor(mbuttfg) -font $guivar(font) \
      -activebackground $guicolor(mbuttabg) -activeforeground $guicolor(mbuttafg) \
      -highlightthickness 0 \
      -padx 2 -pady 2 ]

  settings:init_contents $f1
  pack configure $f1   -fill both -expand yes -side top -padx 3 -pady 3

  pack configure $b_ok -padx 3 -pady 3
  pack configure $f2   -fill x -side top
  
  return $top

} ;# Fin proc settings:init_gui


#----------------------------------------------------------------------
# settings:init_contents
#----------------------------------------------------------------------
proc settings:init_contents { fr } {
  global projet guivar guicolor

  label $fr.l0 -text "master document :"
  entry $fr.e0 -textvariable projet(mainfile)

  pack configure $fr.l0 $fr.e0 -side left -padx 3 -pady 3

  return 

} ;# Fin proc settings:init_contents


#----------------------------------------------------------------------
# settings:Ok
#----------------------------------------------------------------------
proc settings:Ok {} {
  global guivar

  puts stdout "Validation des paramètres"
  destroy $guivar(settings)

} ;# Fin proc settings:Ok


