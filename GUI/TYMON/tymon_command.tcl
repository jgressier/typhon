#----------------------------------------------------------------------
# tymon_command.tcl                                  Jérémie Gressier
#                                                       July 2004
# Commandes TYMON
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# tymon:settings
#----------------------------------------------------------------------
proc tymon:settings {} {
  global projet guivar

  puts stdout "Action en cours d'implémentation"

  set guivar(settings) [settings:init_gui]

} ;# Fin proc tymon:settings


#----------------------------------------------------------------------
# tymon:compile
#----------------------------------------------------------------------
proc tymon:compile {} {
  global projet

  puts stdout "Compile: Action non implémentée"

} ;# Fin proc tymon:compile


#----------------------------------------------------------------------
# tymon:quit
#----------------------------------------------------------------------
proc tymon:quit {} {
  global projet

  set rep 1   ;# quitter sans sauver

  if {$projet(save) == 0} { 
    set rep [tk_dialog .diag "Caution" \
      "You are exiting without saving parameters !" \
      "" 0 "Save & Exit" "Quit w/o saving" "Cancel exit" ] 
  }
  
  if {$rep == 0} { tymon:saveproject }
  
  if {$rep != 2} {
    tymon:writeconf
    puts "-- Fin de l'application --"
    exit
  }

} ;# Fin proc tymon:quit


#----------------------------------------------------------------------
# tymon:exec_node
#----------------------------------------------------------------------
proc tymon:exec_node { node } {
  global guivar

  tymon:edit [$guivar(tree) itemcget $node -data]

} ;# Fin proc tymon:exec_node


#----------------------------------------------------------------------
# tymon:select_node
#----------------------------------------------------------------------
proc tymon:select_node { node } {
  global projet guivar

  

} ;# Fin proc tymon:select_node


#----------------------------------------------------------------------
# tymon:edit
#----------------------------------------------------------------------
proc tymon:edit { file } {
  global settings

  puts stdout "$settings(editeur) \"$file\""
  catch [eval "exec $settings(editeur) \"$file\" &"]

} ;# Fin proc tymon:select_node











