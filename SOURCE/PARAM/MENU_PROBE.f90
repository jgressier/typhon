!------------------------------------------------------------------------------!
! MODULE : MENU_PROBE                  Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition des capteurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_PROBE

use TYPHMAKE   
use GEO3D      
use FCT_NODE   ! symbolic functions

implicit none

! -- Variables globales du module -------------------------------------------

! -- type de capteur --
integer(kpp), parameter :: probe_cell    = 01    ! value in a cell
integer(kpp), parameter :: residuals     = 05    ! residuals
integer(kpp), parameter :: boco_field    = 10    ! 
integer(kpp), parameter :: boco_integral = 11    ! sum on a boco family
integer(kpp), parameter :: vol_average   = 21    ! average in internal volume
integer(kpp), parameter :: vol_min       = 22    ! min     in internal volume
integer(kpp), parameter :: vol_max       = 23    ! max     in internal volume


! -- type de stockage --
integer(kpp), parameter :: no_store   = 1       ! stockage momentanne de l'iteration uniquement
integer(kpp), parameter :: prb_cycle  = 2       ! a chaque cycle
integer(kpp), parameter :: prb_iter   = 3       ! pour chaque iteration interne de zone


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_PROBE : options numeriques les capteurs
!------------------------------------------------------------------------------!
type mnu_probe
  integer(kpp)          :: type        ! type de capteur
  integer(kpp)          :: store       ! type de stockage
  logical               :: write       ! ecriture des donnees
  character(len=shortname) :: name        ! 
  character(len=shortname) :: boco_name   ! famille associee (si necessaire)
                                       !   DEV: on peut extrapoler a plusieurs familles
                                       !        ou proposer la fusion de condition limite dans MESH
  integer               :: boco_index  ! index de condition limite
  type(st_fct_node)     :: quantity    ! quantite a calculer (selon solveur)
  real(krp)             :: result
  type(v3d)             :: center, dir ! vecteurs centre et direction (si necessaire)
endtype mnu_probe


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_probe
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_PROBE
!------------------------------------------------------------------------------!
subroutine delete_mnu_probe(defcapteur)
implicit none
type(mnu_probe)  :: defcapteur

  !print*,'!! DEBUG destruction de structure "parametres" a completer'

endsubroutine delete_mnu_probe



endmodule MENU_PROBE
!------------------------------------------------------------------------------!
! Changes history
!
! juil 2003: Created
! June 2009: add volumic computation
!------------------------------------------------------------------------------!




