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

integer(kpp), parameter :: split_none     = 0
integer(kpp), parameter :: split_svm2quad = 21
integer(kpp), parameter :: split_svm2tri  = 22
integer(kpp), parameter :: split_svm3wang = 31
integer(kpp), parameter :: split_svm3kris = 32
integer(kpp), parameter :: split_svm3kris2 = 33
integer(kpp), parameter :: split_svm4wang = 41

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_MESH : parametres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mesh
  character             :: format      ! cf VARCOM
  character(len=strlen) :: fichier     ! nom de fichier
  real(krp)             :: scale       ! scale factor
  integer(kpp)          :: splitmesh   ! split method
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



