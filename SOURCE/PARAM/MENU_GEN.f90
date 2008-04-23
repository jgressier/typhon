!------------------------------------------------------------------------------!
! MODULE : MENU_GEN                       Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : cf historique
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options generales
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_GEN

use TYPHMAKE   ! Definition de la precision
use MENU_INVERSE

implicit none

! -- Module constants -------------------------------------------

integer(kpp), parameter :: act_compute = 1
integer(kpp), parameter :: act_analyse = 2

integer(kpp), parameter :: write_end   = 10
integer(kpp), parameter :: write_cycle = 20

! -- Constants for output formats --

character, parameter   :: fmt_CGNS    = 'C'   ! format CGNS
character, parameter   :: fmt_TYPHMSH = 'M'   ! format TYPHON MESH
character, parameter   :: fmt_TECPLOT = 'T'   ! format TECPLOT (ascii)
character, parameter   :: fmt_VIGIE   = 'V'   ! format VIGIE
character, parameter   :: fmt_VTK     = 'K'   ! format VTK
character, parameter   :: fmt_VTKBIN  = 'I'   ! format VTK Binary

! -- Constants for TIME INEGRATION management

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
  character       :: typ_coord  ! type of framework
  character       :: time_model ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  real(krp)       :: duration   ! time duration (if unsteady)
  real(krp)       :: tpsbase    ! pas de temps de base du couplage
  integer(kip)    :: ncycle     ! number of cycles (steady)
  real(krp)       :: residumax  ! valeur maximale du residu admise (stationnaire)
  real(krp)       :: dtbase     ! pas de temps de base d'un cycle
  integer(kpp)    :: action     ! give main action to do
  type(mnu_inv)   :: inverse    ! parameters for inverse method
endtype mnu_project

!------------------------------------------------------------------------------!
! structure MNU_OUTPUT : Parametres du projet
!------------------------------------------------------------------------------!
type mnu_OUTPUT
  character       :: format     ! format de la sortie
  character(len=strlen) &
                  :: fichier    ! nom du fichier de sortie
  integer         :: type       ! type de sortie (cf. VARCOM)
  integer(kpp)    :: write      ! writing moment
  integer         :: period     ! writing period
endtype mnu_OUTPUT

! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_GEN

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation du module
!------------------------------------------------------------------------------!



