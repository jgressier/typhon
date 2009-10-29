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

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTBOCO : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_ustboco
  character(len=shortname)       :: family     ! nom de famille
  integer                        :: idefboco   ! index pointer to defsolver boco definition
                                               !   if <= 0 then other definition (cf defboco_* constants)
  integer                        :: nface      ! nombre de faces concernees
  integer, dimension(:), pointer :: iface      ! liste des faces concernees par
                                               ! les conditions aux limites
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

  call delete_genericfield(bc%avg_quant)
  call delete_genericfield(bc%sum_flux)

  call delete_chainedgfield(bc%bocofield)

endsubroutine delete_ustboco


endmodule DEF_USTBOCO

!------------------------------------------------------------------------------!
! History
!
! Apr  2008: created from USTMESH
!------------------------------------------------------------------------------!
