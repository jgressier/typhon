!------------------------------------------------------------------------------!
! MODULE : MENU_MESH                      Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la lecture de maillage
!
! Defauts/Limitations/Divers :
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
endtype mnu_mesh



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_MESH




