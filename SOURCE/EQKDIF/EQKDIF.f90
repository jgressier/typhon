!------------------------------------------------------------------------------!
! MODULE : EQKDIF                         Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : cf historique
!   Bibliotheque de procedures et fonctions pour la definition des etats
!   dans une equation de diffusion
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module EQKDIF

use TYPHMAKE   ! Definition de la precision
use OUTPUT
use MENU_KDIF
use MATERIAU

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_KDIFETAT : etat physique
!------------------------------------------------------------------------------!
type st_kdifetat
  !real(krp), dimension(:), pointer &
  real(krp)       :: temperature    ! temperatures (nbtemp)
endtype st_kdifetat

! -- INTERFACES -------------------------------------------------------------

!interface new
!  module procedure new_mesh, new_field, new_block, new_zone
!endinterface

!interface delete
!  module procedure delete_mesh, delete_field, delete_block, delete_zone
!endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains



!------------------------------------------------------------------------------!
! Fonction : conversion de variables conservatives en variables primitives
!------------------------------------------------------------------------------!
!type(st_kdifetat) function cons2kdif(defkdif, etat)
!implicit none
! declaration des entrees
!type(mnu_kdif)          :: defkdif
!real(krp), dimension(*) :: etat
!
!  select case(defkdif%materiau%type)
!  case(mat_LIN, mat_KNL)
!    cons2kdif%temperature = etat(1)/defkdif%materiau%Cp
!  case(mat_XMAT)
!    call erreur("Calcul de materiau","Materiau non lineaire interdit")
!  endselect
!
!endfunction cons2kdif


endmodule EQKDIF

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov   2002 : creation du module
! sept  2003 : suppression de la fonction cons2kdif (cf calc_varprim_kdif.f90)
!------------------------------------------------------------------------------!
