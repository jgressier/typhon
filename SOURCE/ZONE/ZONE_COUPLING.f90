!------------------------------------------------------------------------------!
! MODULE : ZONE_COUPLING                  Auteur : E. Radenac / J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juillet 2003
!   Definition des methodes de couplage entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module ZONE_COUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques
use DEFFIELD   ! Definition des champs de valeurs physiques pour les transferts
use VARCOM

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_ZONECOUPLING : structures d'echanges entres zones
!------------------------------------------------------------------------------!
type st_zonecoupling
  integer                    :: solvercoupling ! types de solvers couples
  integer                    :: nface_int      ! nb de faces côte interne
  integer                    :: nface_ext      ! nb de faces côte externe
  type(st_genericfield)      :: echdata        ! donnees d'echange (champ de zone externe)
  type(st_genericfield)      :: etatcons       ! energie a l'interface
  integer, dimension(:), pointer & 
                             :: connface       ! connectivite de face (dim = nface_int)
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
! Procedure : creation d'une structure ZONE_COUPLING
!------------------------------------------------------------------------------!
subroutine new_zcoupling(zc, nfaceint)
implicit none
type(st_zonecoupling)  :: zc
integer                :: nfaceint

zc%nface_int = nfaceint
allocate(zc%connface(nfaceint))

select case(zc%solvercoupling)

  case(kdif_kdif)
    call new_genfield(zc%echdata, nfaceint, 2,1,0)
    call new_genfield(zc%etatcons, nfaceint, 3, 0, 0)
    call init_genericfield(zc%echdata, 0._krp, v3d(0._krp, 0._krp, 0._krp))
    call init_genericfield(zc%etatcons, 0._krp, v3d(0._krp, 0._krp, 0._krp))

  case(kdif_ns)
    call new_genfield(zc%echdata, nfaceint, 2,1,0)
    call new_genfield(zc%etatcons, nfaceint, 3, 0, 0)
    call init_genericfield(zc%echdata, 0._krp, v3d(0._krp, 0._krp, 0._krp))
    call init_genericfield(zc%etatcons, 0._krp, v3d(0._krp, 0._krp, 0._krp))

  case(ns_ns)
    call erreur("incoherence interne (ZONE_COUPLING)", "cas non implemente")

  case default
    call erreur("incoherence interne (ZONE_COUPLING)", &
                "couplage solveurs inconnu")

endselect

endsubroutine new_zcoupling

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure ZONE_COUPLING
!------------------------------------------------------------------------------!
subroutine delete_zcoupling(zc)
implicit none
type(st_zonecoupling)  :: zc

call delete(zc%echdata)

call delete(zc%etatcons)

deallocate(zc%connface)

endsubroutine delete_zcoupling

endmodule ZONE_COUPLING


!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): creation du module
!                      creation de new et delete
! juillet 2003       : ajout de etatcons
!------------------------------------------------------------------------------!


