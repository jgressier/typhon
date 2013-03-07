!------------------------------------------------------------------------------!
! MODULE : MESH_PARAMS                      Authors : J. Gressier
!                                         Created : November 2002
! Fonction
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la lecture de maillage
!
!------------------------------------------------------------------------------!

module MESHPARAMS

USE IOCFD
use VEC3D
use TENSOR3
use FCT_NODE

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: split_none      = 0
integer(kpp), parameter :: split_iso_tri   = 11
integer(kpp), parameter :: split_quad2x2   = 12
integer(kpp), parameter :: split_quad3x3   = 13   ! uniform split
integer(kpp), parameter :: split_quad3x3lg = 14   ! Legendre-Gauss 
integer(kpp), parameter :: split_svm2quad  = 21
integer(kpp), parameter :: split_svm2tri   = 22
integer(kpp), parameter :: split_svm3wang  = 31
integer(kpp), parameter :: split_svm3kris  = 32
integer(kpp), parameter :: split_svm3kris2 = 33
integer(kpp), parameter :: split_svm4wang  = 41
integer(kpp), parameter :: split_svm4kris  = 42
integer(kpp), parameter :: split_svm4kris2 = 43

! -- Constants for Mesh TYPE --

integer(kpp), parameter :: geo_1D     = 5
integer(kpp), parameter :: geo_2D     = 10
integer(kpp), parameter :: geo_2Daxi  = 15
integer(kpp), parameter :: geo_2Dcurv = 20
integer(kpp), parameter :: geo_3D     = 30

! -- Constants for INPUT/OUTPUT formats/location --

character, parameter   :: fmt_CGNS        = 'C'   ! format CGNS
character, parameter   :: fmt_CGNS_linked = 'D'   ! format CGNS with common linked mesh
character, parameter   :: fmt_TYPHON      = 'Y'   ! format TYPHON MESH/solution
character, parameter   :: fmt_AUTOBLOCK   = 'A'   ! internal auto-blocking
character, parameter   :: fmt_TECPLOT     = 'T'   ! format TECPLOT (ascii)
character, parameter   :: fmt_VIGIE       = 'V'   ! format VIGIE
character, parameter   :: fmt_VTK         = 'K'   ! format VTK (ascii)
character, parameter   :: fmt_VTKBIN      = 'I'   ! format VTK Binary

! -- Constants for PERIODICITY definition

integer(kpp), parameter :: per_trans = 2
integer(kpp), parameter :: per_rot   = 3


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_PERIODICITY : Periodicity parameters
!------------------------------------------------------------------------------!
type mnu_periodicity
  character(len=shortname) :: name
  integer(kpp)             :: type
  type(v3d)                :: origin, axis, distance
  real(krp)                :: angle
endtype mnu_periodicity


!------------------------------------------------------------------------------!
! structure MNU_MESHSPLIT : mesh splitting parameters
!------------------------------------------------------------------------------!
type mnu_meshsplit
  integer(kpp)          :: splitmesh            ! split method
  integer(kip)          :: nsplit               ! number of split application
  integer(kip)          :: subcell              ! number of subcells (CV for SVM) 
  integer(kip)          :: svface_split         ! number of subface (CV face) by original face
  integer(kip)          :: intnode              ! number of internal added nodes for cell splitting
  integer(kip)          :: internal_faces       ! number of internal faces (by cell)
  real(krp)             :: splitparam           ! real split parameter
endtype mnu_meshsplit

!------------------------------------------------------------------------------!
! structure MNU_MESH : mesh parameters
!------------------------------------------------------------------------------!
type mnu_mesh
  character               :: format      ! cf 
  character(len=longname) :: filename    ! nom de fichier
  integer(kpp)            :: geo         ! geometric param
  integer(kip)            :: ni, nj, nk  ! size of mesh if autoblocking 
  real(krp)               :: lx, ly, lz  ! dimension of block if autoblocking 
  integer(kip)            :: nperiodicity ! number of periodicity
  type(mnu_periodicity), pointer :: periodicity(:) 
  integer                 :: icgnsbase   ! base index in CGNS file
  integer                 :: icgnszone   ! zone index in CGNS file
  logical                 :: scaling, morphing
  real(krp)               :: scale       ! scale factor
  type(st_fct_node)       :: morph_x, morph_Y, morph_z
  type(mnu_meshsplit)     :: defsplit
  integer(kip)            :: nfgauss     ! number of Gauss points per face
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


!-------------------------------------------------------------------------
! init splitting parameters
!-------------------------------------------------------------------------
subroutine init_splitparam(defsplit)
implicit none
! -- parameters --
type(mnu_meshsplit)    :: defsplit
! -- body --

select case(defsplit%splitmesh)
case(split_none)
  ! nothing to do
case(split_iso_tri)
  defsplit%subcell        = 4      ! nb of CV in SV
  defsplit%intnode        = 0      ! nb of internal added nodes for cell splitting
  defsplit%svface_split   = 2      ! nb of CV face per SV face
  defsplit%internal_faces = 3      ! number of internal faces (by cell)
case(split_svm2quad)
  defsplit%subcell        = 3      ! nb of CV in SV
  defsplit%intnode        = 1      ! nb of internal added nodes for cell splitting
  defsplit%svface_split   = 2      ! nb of CV face per SV face
  defsplit%internal_faces = 3      ! number of internal faces (by cell)
case(split_quad2x2)
  defsplit%subcell        = 4      ! nb of CV in SV
  defsplit%intnode        = 1      ! nb of internal added nodes for cell splitting
  defsplit%svface_split   = 2      ! nb of CV face per SV face
  defsplit%internal_faces = 4      ! number of internal faces (by SV cell)
case(split_quad3x3)
  defsplit%subcell        = 9      ! nb of CV in SV
  defsplit%intnode        = 4      ! nb of internal added nodes for cell splitting
  defsplit%svface_split   = 3      ! nb of CV face per SV face
  defsplit%internal_faces = 12     ! number of internal faces (by SV cell)
case(split_svm3wang, split_svm3kris, split_svm3kris2)
  defsplit%subcell        = 6      ! nb of CV in SV
  defsplit%intnode        = 4      ! nb of internal added nodes for cell splitting
  defsplit%svface_split   = 3      ! nb of CV face per SV face
  defsplit%internal_faces = 9      ! number of internal faces (by cell)
case(split_svm4wang)
  defsplit%subcell        = 10      ! nb of CV in SV
  defsplit%svface_split   = 4      ! nb of CV face per SV face
  defsplit%intnode        = 6      ! nb of internal added nodes for cell splitting
  defsplit%internal_faces = 15      ! number of internal faces (by cell)
case(split_svm4kris)
  defsplit%subcell        = 10      ! nb of CV in SV
  defsplit%svface_split   = 4      ! nb of CV face per SV face
  defsplit%intnode        = 9      ! nb of internal added nodes for cell splitting
  defsplit%internal_faces = 18      ! number of internal faces (by cell)
case(split_svm4kris2)
  defsplit%subcell        = 10      ! nb of CV in SV
  defsplit%svface_split   = 4      ! nb of CV face per SV face
  defsplit%intnode        = 9      ! nb of internal added nodes for cell splitting
  defsplit%internal_faces = 18      ! number of internal faces (by cell)
case default
  call cfd_error("parameters parsing: unknown splitting method (init_splitparam)")
endselect

endsubroutine init_splitparam


endmodule MESHPARAMS
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! sept 2005 : add scale factor
! Dec  2010 : TYPHON (MENU_MESH) -> CFDTOOLS (MESHPARAMS)
!------------------------------------------------------------------------------!



