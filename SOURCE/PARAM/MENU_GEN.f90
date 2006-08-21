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

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_PROJECT : Parametres du projet
!------------------------------------------------------------------------------!
type mnu_project
  integer(kip)    :: nzone      ! nombre de zones
  integer(kip)    :: ncoupling  ! nombre de couplages entre zones
  character       :: typ_coord  ! type de repere
  character       :: typ_temps  ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  real(krp)       :: duree      ! duree de l'integration ou de la periode
  real(krp)       :: tpsbase    ! pas de temps de base du couplage
  integer(kip)    :: ncycle     ! nombre de cycle (en stationnaire ou periodique)
  real(krp)       :: residumax  ! valeur maximale du residu admise (stationnaire)
  real(krp)       :: dtbase     ! pas de temps de base d'un cycle
  integer(kpp)    :: action     ! give main action to do
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



