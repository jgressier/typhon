!------------------------------------------------------------------------------!
! MODULE : MENU_ZONECOUPLING              Auteur : E. Radenac / J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Définition des méthodes de couplage entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_ZONECOUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques
use DEFFIELD   ! Définition des champs de valeurs physiques pour les transferts
use ZONE_COUPLING

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure MNU_ZONECOUPLING : structures d'échanges entres zones
!------------------------------------------------------------------------------!
type mnu_zonecoupling
  character(len=strlen)      :: family     ! nom de famille de la CL
  character(len=strlen)      :: connzone   ! nom de la zone connectée par ce raccord
  character(len=strlen)      :: connfam    ! nom de famille de la CL connectée
                                           ! par ce raccord
  type(st_zonecoupling)      :: zcoupling  ! paramètres de couplage
  real(krp)                  :: partcor    ! part de correction à faire par 
                                           ! itération <=1
  integer                    :: typ_cor    ! type de répartition de correction
endtype mnu_zonecoupling

! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_zonecoupling
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure MNU_ZONECOUPLING
!------------------------------------------------------------------------------!
subroutine delete_zonecoupling(zcoupl)
implicit none
type(mnu_zonecoupling)  :: zcoupl

call delete(zcoupl%zcoupling)

endsubroutine delete_zonecoupling

endmodule MENU_ZONECOUPLING

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création du module
!                      création de delete
!------------------------------------------------------------------------------!


