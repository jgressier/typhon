#----------------------------------------------------------------------
# tymon.tcl                                          Jérémie Gressier
#                                                       
# Interface graphique pour la gestion d'un projet LATEX
#----------------------------------------------------------------------
# 06/04 : création
#----------------------------------------------------------------------

proc tymon:init {} {
  global env guivar projet

  # -- Version --
  set guivar(version)  0.1
  puts stdout "-- TYMON release $guivar(version) --"

  # -- HOME de l'application TYMON GUI --
  if { ![info exists env(GUI_HOME)] } {
    set guivar(HOME) [pwd]
  } else {
    set guivar(HOME) $env(GUI_HOME)
  }

  puts stdout [file join $guivar(HOME) tymon_init.tcl]
  source [file join $guivar(HOME) tymon_init.tcl]
  gui:splash_statut "Initialisation..."
  gui:splash
  tymon:loadlib

} ;# Fin proc tymon:init


#----------------------------------------------------------------------
# CORPS DU PROGRAMME

tymon:init
#wm withdraw .splash

#toplevel .
tymon:initproject
gui:init {}

after 1000 "destroy .splash"
wm deiconify .

tymon:process
tymon:loadproject { tymon.prj }

# FIN
