#----------------------------------------------------------------------
# gui_update.tcl                                     Jérémie Gressier
#                                                      Kdécembre 2003
# Mise à jour de l'interface graphique pour TKTEX
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# gui:update : Mise à jour de l'interface graphique utilisateur
#----------------------------------------------------------------------
proc gui:update {  } {
  global guivar

  #puts stdout $guivar(mode)
  update

} ;# fin proc gui:update


#----------------------------------------------------------------------
# gui:update_font
#----------------------------------------------------------------------
proc gui:update_font {} {
  global guivar
  
  . configure -cursor watch

  # Généralisation
  foreach w $guivar(wlist4font) { $w configure -font $guivar(font) }

  # Cas particulier
  $guivar(projetlabel) configure -labelfont $guivar(font)
  foreach tab [$guivar(tabset) tab names] {
    $guivar(tabset) tab configure $tab -font $guivar(font)
  }

  . configure -cursor ""

} ;# fin proc gui:update_font


#----------------------------------------------------------------------
# gui:setstatut
#----------------------------------------------------------------------
proc gui:setstatut { mess } {
  global guivar

  set guivar(statut) $mess

} ;# fin proc gui:setstatut





