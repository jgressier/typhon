!------------------------------------------------------------------------------!
! MODULE : MENU_MESH                      Authors : J. Gressier
!                                         Created : November 2002
! Fonction
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la lecture de maillage
!
!------------------------------------------------------------------------------!

module MENU_MESH

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_MESH : parametres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mesh
  character             :: format      ! cf VARCOM
  character(len=strlen) :: fichier     ! nom de fichier
  real(krp)             :: scale       ! scale factor
endtype mnu_mesh



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_MESH
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! sept 2005 : add scale factor
!------------------------------------------------------------------------------!



