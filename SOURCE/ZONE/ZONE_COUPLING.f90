!------------------------------------------------------------------------------!
! MODULE : ZONE_COUPLING                  Auteur : E. Radenac / J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juillet 2003
!   Définition des méthodes de couplage entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module ZONE_COUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques
use DEFFIELD   ! Définition des champs de valeurs physiques pour les transferts

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_ZONECOUPLING : structures d'échanges entres zones
!------------------------------------------------------------------------------!
type st_zonecoupling
  integer                    :: solvercoupling ! types de solvers couplés
  integer                    :: nface_int      ! nb de faces côté interne
  integer                    :: nface_ext      ! nb de faces côté externe
  type(st_genericfield)      :: echdata        ! données d'échange (champ de zone externe)
  type(st_genericfield)      :: etatcons       ! énergie à l'interface
  integer, dimension(:), pointer &
                             :: connface       ! connectivité de face (dim = nface_int)
                                               !   connface(i) = j
                                               !   : face i interne = face j externe
endtype st_zonecoupling

! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_zcoupling
endinterface

interface delete
  module procedure delete_zcoupling
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procédure : création d'une structure ZONE_COUPLING
!------------------------------------------------------------------------------!
subroutine new_zcoupling(zc, nfaceint)
implicit none
type(st_zonecoupling)  :: zc
integer                :: nfaceint

zc%nface_int = nfaceint
allocate(zc%connface(nfaceint))

endsubroutine new_zcoupling

!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure ZONE_COUPLING
!------------------------------------------------------------------------------!
subroutine delete_zcoupling(zc)
implicit none
type(st_zonecoupling)  :: zc

call delete(zc%echdata)

call delete(zc%etatcons)

!deallocate(zc%connface)

endsubroutine delete_zcoupling

endmodule ZONE_COUPLING


!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création du module
!                      création de new et delete
! juillet 2003       : ajout de etatcons
!------------------------------------------------------------------------------!


