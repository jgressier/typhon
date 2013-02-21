!------------------------------------------------------------------------------!
! MODULE : DEFFIELD                             Authors : J. Gressier
!
! Function
!   Library for managing scalar, vector, tensor fields and the derivatives
!   of variables type (conservative, primitive, residuals, gradients)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!

module DEFFIELD

use TYPHMAKE     ! Precision and string size
use OUTPUT
use GEO3D        !
use TENSOR3      !
use GENFIELD

implicit none

! -- Module global variables ------------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! ST_FIELD structure definition : physical and derived fields
!------------------------------------------------------------------------------!

type st_field
  integer                 :: id              ! id number of field
  type(st_field), pointer :: next            ! pointer in chained list
  integer                 :: nscal, nvect    ! dimension of PDE problem
  integer                 :: ncell, nface    ! number of cells and faces
  integer                 :: ndof,  nfgauss   ! dimension of cell and face representation 
  logical                 :: allocqref       ! allocation of qref field
  logical                 :: allocgrad       ! allocation of cell averaged gradients
  logical                 :: allocres        ! allocation of residuals
  logical                 :: allocprim       ! allocation of primitive variables
  logical                 :: allocqhres      ! allocation of high order extrapolated states
  logical                 :: allocfacegrad   ! allocation of face extrapolated gradients
  logical                 :: calcgrad        ! use of gradients
  type(st_genericfield)   :: qref            ! reference field
  type(st_genericfield)   :: etatcons        ! conservative variables
  type(st_genericfield)   :: etatprim        ! primitive variables
  type(st_genericfield)   :: gradient        ! gradients of primitive variables
  type(st_genericfield)   :: cell_l, cell_r  ! high order extrapolated states
  type(st_genericfield)   :: grad_l, grad_r  ! high order extrapolated gradients
  type(st_genericfield)   :: residu          ! residuals of conservative variables
endtype st_field



! -- INTERFACES -------------------------------------------------------------

!interface new ! interfaces are not so readable in software dev
!  module procedure new_field
!endinterface

interface delete  ! 
  module procedure delete_field
endinterface

! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : gradients allocation
!------------------------------------------------------------------------------!
subroutine alloc_cellgrad(field)
implicit none
type(st_field) :: field

  if (field%allocgrad) then
    ! NO MESSAGE // call print_info(90,"!!! gradients array already allocated !!!")
  else
    field%allocgrad = .true.
    call new_genfield(field%gradient, field%etatcons%dim, 0, field%etatcons%nscal, field%etatcons%nvect)
  endif

endsubroutine alloc_cellgrad


!------------------------------------------------------------------------------!
! Procedure : gradients deallocation
!------------------------------------------------------------------------------!
subroutine dealloc_cellgrad(field)
implicit none
type(st_field) :: field

  if (field%allocgrad) then
    call delete(field%gradient)
    field%allocgrad = .false.
  else
    call print_info(90,"!!! unable to deallocate : gradients array not allocated !!!")
  endif

endsubroutine dealloc_cellgrad


!------------------------------------------------------------------------------!
! Procedure : residuals allocation
!------------------------------------------------------------------------------!
subroutine alloc_res(field)
implicit none
type(st_field) :: field

  if (field%allocres) then
    ! NO MESSAGE // call print_info(90,"!!! residuals array already allocated !!!")
  else
    field%allocres = .true.
    call new_genfield(field%residu, field%etatcons%dim,   field%etatcons%nscal, &
                           field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_res


!------------------------------------------------------------------------------!
! Procedure : residuals deallocation
!------------------------------------------------------------------------------!
subroutine dealloc_res(field)
implicit none
type(st_field) :: field

  if (field%allocres) then
    call delete(field%residu)
    field%allocres = .false.
  else
    call print_info(90,"!!! unable to deallocate : residuals array not allocated !!!")
  endif

endsubroutine dealloc_res


!------------------------------------------------------------------------------!
! Procedure : primitive variables allocation
!------------------------------------------------------------------------------!
subroutine alloc_prim(field)
implicit none
type(st_field) :: field

  if (field%allocprim) then
    call print_info(90,"!!! primitive variables array already allocated !!!")
  else
    field%allocprim = .true.
    call new_genfield(field%etatprim, field%etatcons%dim,   field%etatcons%nscal, &
                             field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_prim


!------------------------------------------------------------------------------!
! Procedure : primitive variables deallocation
!------------------------------------------------------------------------------!
subroutine dealloc_prim(field)
implicit none
type(st_field) :: field

  if (field%allocprim) then
    field%allocprim = .false.
    call delete(field%etatprim)
  else
    call print_info(90,"!!! unable to deallocate : primitive variables array not allocated !!!")
  endif

endsubroutine dealloc_prim


!------------------------------------------------------------------------------!
! Procedure : allocation of high order extrapolated states
!------------------------------------------------------------------------------!
subroutine alloc_hres_states(field)
implicit none
type(st_field) :: field

  if (field%allocqhres) then
    !  call cfd_error("Fatal allocation error: high order extrapolated array already allocated with bad size")
    ! supposed to be allocated at same size
  else
    field%allocqhres = .true.
    call new_genfield(field%cell_l, field%nface, field%etatcons%nscal, field%etatcons%nvect, field%etatcons%ntens)
    call new_genfield(field%cell_r, field%nface, field%etatcons%nscal, field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_hres_states


!------------------------------------------------------------------------------!
! Procedure : deallocation of high order extrapolated states
!------------------------------------------------------------------------------!
subroutine dealloc_hres_states(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocqhres) then
    field%allocqhres = .false.
    call delete(field%cell_l)
    call delete(field%cell_r)
  else
    call print_info(90,"!!! unable to deallocate : face based gradient array not allocated !!!")
  endif

endsubroutine dealloc_hres_states


!------------------------------------------------------------------------------!
! Procedure : allocation of face gradients
!------------------------------------------------------------------------------!
subroutine alloc_facegrad(field)
implicit none
type(st_field) :: field

  if (field%allocfacegrad) then
    !  call cfd_error("Fatal allocation error: face gradients already allocated with bad size")
    ! supposed to be allocated at same size
  else
    field%allocfacegrad = .true.
    call new_genfield(field%grad_l, field%nface, 0, field%etatcons%nscal, field%etatcons%nvect)
    call new_genfield(field%grad_r, field%nface, 0, field%etatcons%nscal, field%etatcons%nvect)
  endif

endsubroutine alloc_facegrad


!------------------------------------------------------------------------------!
! Procedure : deallocation of face gradients
!------------------------------------------------------------------------------!
subroutine dealloc_facegrad(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocfacegrad) then
    field%allocfacegrad = .false.
    call delete(field%grad_l)
    call delete(field%grad_r)
  else
    call print_info(90,"!!! unable to deallocate : face gradients array not allocated !!!")
  endif

endsubroutine dealloc_facegrad


!------------------------------------------------------------------------------!
! Procedure : reference variables allocation
!------------------------------------------------------------------------------!
subroutine alloc_qref(field)
implicit none
type(st_field) :: field

  if (field%allocqref) then
    call print_info(90,"!!! reference variables array already allocated !!!")
  else
    field%allocqref = .true.
    call new_genfield(field%qref, field%etatcons%dim,   field%etatcons%nscal, &
                         field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_qref


!------------------------------------------------------------------------------!
! Procedure : reference variables deallocation
!------------------------------------------------------------------------------!
subroutine dealloc_qref(field)
implicit none
type(st_field) :: field

  if (field%allocqref) then
    field%allocqref = .false.
    call delete(field%qref)
  else
    call print_info(90,"!!! unable to deallocate : reference variables array not allocated !!!")
  endif

endsubroutine dealloc_qref


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine new_field(field, id, n_scal, n_vect, ncell, nface, ndof, nfgauss)
implicit none
type(st_field)    :: field             ! champ a creer
integer           :: ncell, nface      ! nombre de cellules et faces
integer           :: n_scal, n_vect    ! nombre de scalaires, vecteurs et tenseurs
integer, optional :: ndof, nfgauss      ! 
integer           :: id                ! numero de champ

  field%id        = id
  field%ncell     = ncell
  field%nface     = nface
  field%nscal     = n_scal
  field%nvect     = n_vect
  if (present(ndof)) then
    field%ndof = ndof
  else   
    field%ndof = 1
  endif
  if (present(nfgauss)) then
    field%nfgauss = nfgauss
  else   
    field%nfgauss = 1
  endif

  call new_genfield(field%etatcons, ncell, n_scal, n_vect, 0)
  field%allocgrad     = .false.
  field%allocfacegrad = .false.
  field%allocres      = .false.
  field%allocprim     = .false.
  field%allocqref     = .false.
  field%allocQhres    = .false.
  field%allocfacegrad = .false.
endsubroutine new_field


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine delete_field(field)
implicit none
type(st_field) :: field             ! champ a creer

  call delete(field%etatcons)

  if (field%allocgrad)  call dealloc_cellgrad(field)
  if (field%allocres)   call dealloc_res (field)
  if (field%allocprim)  call dealloc_prim(field)
  if (field%allocqref)  call dealloc_qref(field)
  if (field%allocqhres )   call dealloc_hres_states(field)
  if (field%allocfacegrad) call dealloc_facegrad(field)

endsubroutine delete_field


!------------------------------------------------------------------------------!
! Procedure : creation et lien chaine d'une structure FIELD
!------------------------------------------------------------------------------!
function insert_newfield(field, id, n_scal, n_vect, ncell, nface) result(pfield)
implicit none
type(st_field), pointer :: pfield
type(st_field), target  :: field
integer                 :: n_scal,n_vect,ncell,nface
integer                 :: id

  allocate(pfield)
  call new_field(pfield,id,n_scal,n_vect,ncell,nface)
  pfield%next => field

endfunction insert_newfield


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une liste chainee de structure FIELD
!------------------------------------------------------------------------------!
subroutine delete_chainedfield(field)
implicit none
type(st_field), target  :: field
type(st_field), pointer :: pfield, dfield

  pfield => field
  do while(associated(pfield))
    dfield => pfield
    pfield => pfield%next
    call delete(dfield)
  enddo

endsubroutine delete_chainedfield

!------------------------------------------------------------------------------
! Routine : transfer of ifield to rfield
!------------------------------------------------------------------------------
subroutine transfer_field(rfield, ifield)
implicit none
type(st_field) :: ifield, rfield

rfield%nscal = ifield%nscal
rfield%nvect = ifield%nvect
rfield%ncell = ifield%ncell
rfield%nface = ifield%nface
rfield%allocgrad = ifield%allocgrad
rfield%allocres  = ifield%allocres
rfield%allocprim = ifield%allocprim
rfield%allocqref = ifield%allocqref
rfield%allocqhres = ifield%allocqhres
rfield%calcgrad  = ifield%calcgrad
call transfer(rfield%etatcons,ifield%etatcons)
call transfer(rfield%etatprim,ifield%etatprim)
call transfer(rfield%qref,ifield%qref)
call transfer(rfield%cell_l,ifield%cell_l)
call transfer(rfield%cell_r,ifield%cell_r)
call transfer(rfield%grad_l,ifield%grad_l)
call transfer(rfield%grad_r,ifield%grad_r)
call transfer(rfield%gradient,ifield%gradient)
call transfer(rfield%residu,ifield%residu)

endsubroutine transfer_field


!------------------------------------------------------------------------------
! field_setref : copy etatcons to qref
!------------------------------------------------------------------------------
subroutine field_cons2ref(field)
implicit none
type(st_field) :: field

if (.not.field%allocqref) call alloc_qref(field)

call transfer(field%qref, field%etatcons)

endsubroutine field_cons2ref

!------------------------------------------------------------------------------
! field_setref : copy qref to etatcons
!------------------------------------------------------------------------------
subroutine field_ref2cons(field)
implicit none
type(st_field) :: field

call transfer(field%etatcons, field%qref)

endsubroutine field_ref2cons


endmodule DEFFIELD

!------------------------------------------------------------------------------!
! Changes history
!
! Oct 2002 : creation
! Jun 2003 : field structure by type (scalar, vector, tensor)
! DEV: field/array interface
! DEV: split in MGFIELD and MZFIELD pour low- and high-level functions haut
! Jun 2004 : procedures insert_newgfield and delete_chainedgfield
! Oct 2004 : field chained list
! Nov 2004 : split DEFFIELD -> DEFFIELD/GENFIELD
! Apr 2008 : create qref and routines to copy to/from etatcons
! May 2009 : add cell_l and cell_r states
!------------------------------------------------------------------------------!
