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
use GEO3D

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: split_none      = 0
integer(kpp), parameter :: split_svm2quad  = 21
integer(kpp), parameter :: split_svm2tri   = 22
integer(kpp), parameter :: split_svm3wang  = 31
integer(kpp), parameter :: split_svm3kris  = 32
integer(kpp), parameter :: split_svm3kris2 = 33
integer(kpp), parameter :: split_svm4wang  = 41
integer(kpp), parameter :: split_svm4kris  = 42
integer(kpp), parameter :: split_svm4kris2 = 43

! -- Constants for PERIODICITY defintion

integer(kpp), parameter :: per_trans = 2
integer(kpp), parameter :: per_rot   = 3


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_PERIODICITY : Periodicity parameters
!------------------------------------------------------------------------------!
type mnu_periodicity
  character(len=strlen) :: name
  integer(kpp)          :: type
  type(v3d)             :: origin, axis, distance
  real(krp)             :: angle
endtype mnu_periodicity


!------------------------------------------------------------------------------!
! structure MNU_MESH : parametres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mesh
  character             :: format      ! cf VARCOM
  character(len=strlen) :: filename    ! nom de fichier
  real(krp)             :: scale       ! scale factor
  integer(kpp)          :: splitmesh   ! split method
  integer(kip)          :: nperiodicity ! number of periodicity
  type(mnu_periodicity), pointer :: periodicity(:) 
  integer               :: icgnsbase   ! original base index in CGNS file
  integer               :: icgnszone   ! original zone index in CGNS file
endtype mnu_mesh



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! transloc_per: transfer location according to periodicity definition
!------------------------------------------------------------------------------!
subroutine transloc_per(defper, vec, dir)
implicit none
! --- INPUTS ---
type(mnu_periodicity) :: defper
real(krp)             :: dir
! --- IN/OUTPUTS ---
type(v3d)             :: vec(:)
! --- private data ---
integer :: i, n

n = size(vec)

select case(defper%type)
case(per_trans)
  call shift_add(vec(1:n), dir*defper%distance)
case default
  print*,'unknown type of periodicity (internal)'
  stop
endselect

endsubroutine transloc_per



endmodule MENU_MESH
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! sept 2005 : add scale factor
!------------------------------------------------------------------------------!



