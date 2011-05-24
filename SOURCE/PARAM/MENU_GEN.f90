!------------------------------------------------------------------------------!
! MODULE : MENU_GEN
!
! Fonction
!   Definition of general/project options
!
!------------------------------------------------------------------------------!
module MENU_GEN

use TYPHMAKE
use MENU_INVERSE
use MESHPARAMS
use TYPHON_FMT
use VTKFMT

implicit none

! -- Module constants -------------------------------------------

integer(kpp), parameter :: act_compute = 1
integer(kpp), parameter :: act_analyse = 2

! -- outputs constants --

integer(kpp), parameter :: write_end   = 1
integer(kpp), parameter :: write_cycle = 2

integer(kpp), parameter :: dataset_node     = 05
integer(kpp), parameter :: dataset_cell     = 10
integer(kpp), parameter :: dataset_svcell   = 11
integer(kpp), parameter :: dataset_bococell = 15
integer(kpp), parameter :: dataset_boconode = 16

! -- Constants for TIME INTEGRATION management

character, parameter :: time_steady            = 'S'
character, parameter :: time_unsteady          = 'I'
character, parameter :: time_harmonic          = 'H'
character, parameter :: time_unsteady_periodic = 'P'
character, parameter :: time_unsteady_inverse  = 'V'

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_PROJECT : Project Parameters
!------------------------------------------------------------------------------!
type mnu_project
  integer(kip)    :: nzone      ! number of zones
  integer(kip)    :: ncoupling  ! number of coupling conditions between zones
  integer(kpp)    :: typ_coord  ! type of framework
  character       :: time_model ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  real(krp)       :: duration   ! time duration (if unsteady)
  real(krp)       :: tpsbase    ! pas de temps de base du couplage
  integer(kip)    :: ncycle     ! number of cycles (steady)
  real(krp)       :: residumax  ! valeur maximale du residu admise (steady)
  real(krp)       :: dtbase     ! pas de temps de base d'un cycle
  integer(kpp)    :: action     ! give main action to do
  type(mnu_inv)   :: inverse    ! parameters for inverse method
endtype mnu_project

!------------------------------------------------------------------------------!
! structure MNU_OUTPUT : Output parameters
!------------------------------------------------------------------------------!
type mnu_output
  integer(kpp)            :: dataset       ! data set to save
  integer(kpp)            :: location      ! data location
  integer(kip)            :: index         ! solution index
  integer(xbinkpp)        :: meshdef       ! mesh writing method (see TYFMT_MESH)
  logical                 :: savedmesh     ! reference mesh saved
  character               :: format        ! output format
  character(len=longname) :: basename      ! base name for files
  character(len=longname) :: filename      ! file name from basename
  integer                 :: iunit         ! IO unit
  type(st_deftyphon)      :: deftyphon     ! TYPHON file info
  type(st_defvtk)         :: defvtk        ! VTK    file info
  integer                 :: izone         ! zone index (base for CGNS)
  integer(kpp)            :: write         ! writing instant (end, cycle...)
  integer(kpp)            :: refframe      ! reference frame
  integer                 :: period        ! writing period (if "cycle")
endtype mnu_output

! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_GEN

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002: creation
! May  2008: enhances output structure
!------------------------------------------------------------------------------!



