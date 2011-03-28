!------------------------------------------------------------------------------!
! MODULE : MESH_PARAMS                      Authors : J. Gressier
!                                         Created : November 2002
! Fonction
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la lecture de maillage
!
!------------------------------------------------------------------------------!

module MESHPARAMS

use VEC3D
use TENSOR3

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: split_none      = 0
integer(kpp), parameter :: split_iso_tri   = 11
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
  character(len=shortname) :: name
  integer(kpp)          :: type
  type(v3d)             :: origin, axis, distance
  real(krp)             :: angle
endtype mnu_periodicity


!------------------------------------------------------------------------------!
! structure MNU_MESH : parametres pour la distribution entre processeurs
!------------------------------------------------------------------------------!
type mnu_mesh
  character               :: format      ! cf VARCOM
  character(len=longname) :: filename    ! nom de fichier
  real(krp)               :: scale       ! scale factor
  integer(kpp)            :: splitmesh   ! split method
  integer(kip)            :: nsplit      ! number of split application
  integer(kip)            :: nperiodicity ! number of periodicity
  type(mnu_periodicity), pointer :: periodicity(:) 
  integer                 :: icgnsbase   ! base index in CGNS file
  integer                 :: icgnszone   ! zone index in CGNS file
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
type(v3d), intent(inout) :: vec(:)
! --- private data ---
integer                :: i, n
type(v3d), allocatable :: dv(:)

n = size(vec)

select case(defper%type)
case(per_trans)
  call shift_add(vec(1:n), dir*defper%distance)
case(per_rot)
  call shift_sub(vec(1:n), defper%origin)
  call rot(vec(1:n), defper%axis, dir*defper%angle)
  call shift_add(vec(1:n), defper%origin)
case default
  print*,'unknown type of periodicity (internal)'
  stop
endselect

endsubroutine transloc_per

!------------------------------------------------------------------------------!
! transvec_per: transfer vector according to periodicity definition
!------------------------------------------------------------------------------!
subroutine transvec_per(defper, vec, dir)
implicit none
! --- INPUTS ---
type(mnu_periodicity) :: defper
real(krp)             :: dir
! --- IN/OUTPUTS ---
type(v3d), intent(inout) :: vec(:)
! --- private data ---
integer :: i, n

n = size(vec)

select case(defper%type)
case(per_trans)
  ! nothing to do !
case(per_rot)
  call rot(vec(1:n), defper%axis, dir*defper%angle)
case default
  print*,'unknown type of periodicity (internal)'
  stop
endselect

endsubroutine transvec_per

!------------------------------------------------------------------------------!
! transvec_per: transfer vector according to periodicity definition
!------------------------------------------------------------------------------!
subroutine transten_per(defper, ten, dir)
implicit none
! --- INPUTS ---
type(mnu_periodicity) :: defper
real(krp)             :: dir
! --- IN/OUTPUTS ---
type(t3d), intent(inout) :: ten(:)
! --- private data ---
integer :: i, n

n = size(ten)

select case(defper%type)
case(per_trans)
  ! nothing to do !
case(per_rot)
  call rot(ten(1:n), defper%axis, dir*defper%angle)
case default
  print*,'unknown type of periodicity (internal)'
  stop
endselect

endsubroutine transten_per

endmodule MESHPARAMS
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! sept 2005 : add scale factor
! Dec  2010 : TYPHON (MENU_MESH) -> CFDTOOLS (MESHPARAMS)
!------------------------------------------------------------------------------!



