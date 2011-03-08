!------------------------------------------------------------------------------!
! MODULE : USTBOCO 
!                  
! BOundary COndition structure for USTMESH
!
!------------------------------------------------------------------------------!

module DEF_USTBOCO

use MESHPREC      ! machine accuracy
use VEC3D 
use CONNECTIVITY  ! lists & connectivity 
use GENFIELD
use GRID_CONNECT  ! definition of connectivity between grids

implicit none

! -- CONSTANTS --------------------------------------------------------------

integer, parameter :: defboco_connect = -1       

integer, parameter :: iloc_vtex     = 1
integer, parameter :: iloc_elemcell = 2
integer, parameter :: iloc_elemface = 3
integer, parameter :: iloc_face     = 4

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTBOCO : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_ustboco
  character(len=shortname)       :: family     ! Family name or TAG
  integer                        :: ilocation  ! type of connectivity
  integer                        :: idefboco   ! index pointer to defsolver boco definition
                                               !   if <= 0 then other definition (cf defboco_* constants)
  integer                        :: ntag       ! number of tags
  integer, dimension(:), pointer :: itag       ! list of tags
  integer                        :: nface      ! number of tagged face 
  integer, dimension(:), pointer :: iface      ! face index
  real(krp)                      :: area       ! sum of tagged face area
  type(st_genericfield), pointer :: bocofield  ! pointer to chained list of boco fields (in MGRID)
  type(st_genericfield)          :: avg_quant  ! average quantity on boco 
  type(st_genericfield)          :: sum_flux   ! sum of fluxes    on boco
  type(st_gridconnect)           :: gridcon    ! connectivity to grid
endtype st_ustboco


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustboco
endinterface

interface delete
  module procedure delete_ustboco
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

character(14) function str_location(iloc)
implicit none
integer :: iloc
select case(iloc)
case(iloc_vtex)
  str_location = "vertex"
case(iloc_elemcell)
  str_location = "cell element"
case(iloc_elemface)
  str_location = "face element"
case(iloc_face)
  str_location = "internal face"
case default
  str_location = "unknown"
endselect
endfunction str_location

!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine new_ustboco(bc, nom, n)
implicit none
type(st_ustboco), intent(out) :: bc
character(len=*), intent(in)  :: nom
integer,          intent(in)  :: n

  bc%family = nom
  bc%nface  = n
  allocate(bc%iface(n))
  nullify(bc%bocofield)  
  nullify(bc%itag)
  call new_genericfield(bc%avg_quant, 0, 0, 0, 0)   ! initialization
  call new_genericfield(bc%sum_flux,  0, 0, 0, 0)   ! initialization

  call init_gridconnect(bc%gridcon)

endsubroutine new_ustboco


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine delete_ustboco(bc)
implicit none
type(st_ustboco) :: bc
integer          :: i

  deallocate(bc%iface)
  if (bc%idefboco <= 0) call delete(bc%gridcon)

  if (associated(bc%itag)) deallocate(bc%itag)

  call delete_genericfield(bc%avg_quant)
  call delete_genericfield(bc%sum_flux)

  call delete_chainedgfield(bc%bocofield)

endsubroutine delete_ustboco

!------------------------------------------------------------------------------!
! Procedure : desallocation des tags 
!------------------------------------------------------------------------------!
subroutine deletetag_ustboco(bc)
implicit none
type(st_ustboco) :: bc
integer          :: i

  deallocate(bc%itag)
  bc%ntag = 0

endsubroutine deletetag_ustboco


endmodule DEF_USTBOCO
!------------------------------------------------------------------------------!
! History
!
! Apr  2008: created from USTMESH
!------------------------------------------------------------------------------!
