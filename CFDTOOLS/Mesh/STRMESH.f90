!------------------------------------------------------------------------------!
! MODULE : STRMESH 
!
! Grid geometry and connectivity for unstructured mesh
!------------------------------------------------------------------------------!

module STRMESH

use TYPHMAKE      ! configuration of machine accuracy
use MESHPARAMS

implicit none


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Definition de la structure ST_strmesh : Maillage non structure
!------------------------------------------------------------------------------!
! Organization of face arrays:
!   SVM internal faces (1..nface_svmint)
! les elements limites.

type st_strmesh
  integer(kip)          :: id                    ! domain id
  integer(kip)          :: geodim                ! geometrical dimension
  integer(kip)          :: ni, nj, nk            ! block cell   dimensions
  real(krp)             :: lx, ly, lz            ! block length dimensions
  integer               :: nboco                 ! number of boundary conditions
  !type(st_strboco), dimension(:), pointer &
  !                      :: boco                  ! liste des conditions aux limites
endtype st_strmesh



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_strmesh
endinterface

interface delete
  module procedure delete_strmesh
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : allocation of STRMESH structure
!------------------------------------------------------------------------------!
subroutine new_strmesh(strmesh, ni, nj, nk, lx, ly, lz)
implicit none
type(st_strmesh) :: strmesh
integer(kip)     :: ni, nj, nk
real(krp)        :: lx, ly, lz

  strmesh%ni = ni
  strmesh%nj = nj
  if (nk == 0) then
    strmesh%geodim = 2
    strmesh%nk     = 1
  else
    strmesh%geodim = 3
    strmesh%nk     = nk
  endif  
  strmesh%lx = lx
  strmesh%ly = ly
  strmesh%lz = lz
  
endsubroutine new_strmesh

!------------------------------------------------------------------------------!
! Procedure : Initialization of STRMESH structure
!------------------------------------------------------------------------------!
subroutine init_strmesh(umesh, id)
implicit none
type(st_strmesh) :: umesh
integer(kpp)     :: geotyp
integer(kip)     :: id


endsubroutine init_strmesh

!------------------------------------------------------------------------------!
! Procedure : deallocation of STRMESH structure
!------------------------------------------------------------------------------!
subroutine delete_strmesh(strmesh, ni, nj, nk, lx, ly, lz)
implicit none
type(st_strmesh) :: strmesh
integer(kip)     :: ni, nj, nk
real(krp)        :: lx, ly, lz

  
endsubroutine delete_strmesh


!------------------------------------------------------------------------------!
! Procedure : definition of STRMESH from MNU_MESH
!------------------------------------------------------------------------------!
subroutine create_autoblockmesh(defmesh, strmesh)
implicit none
type(mnu_mesh)   :: defmesh
type(st_strmesh) :: strmesh

  call new_strmesh(strmesh, defmesh%ni, defmesh%nj, defmesh%nk, &
                            defmesh%lx, defmesh%ly, defmesh%lz)
  
endsubroutine create_autoblockmesh



endmodule STRMESH
!------------------------------------------------------------------------------!
! History
!
! Feb  2013 : creation du module
!------------------------------------------------------------------------------!
