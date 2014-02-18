!------------------------------------------------------------------------------!
! Procedure : setboco_ns_inlet_sup        Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction    
!   Computation of supersonic inlet boundary conditions
!   
!------------------------------------------------------------------------------!
subroutine setboco_ns_inlet_sup(defns, mrf, unif, bc_ns, ustboco, umesh, bccon, curtime)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use MGRID 
use FCT_EVAL
use FCT_ENV
use MENU_SOLVER
use MENU_BOCO
use MESHMRF
use EQNS

implicit none

! -- INPUTS --
real(krp)        :: curtime
type(mnu_ns)     :: defns            ! solver parameters
type(mnu_mrf)    :: mrf              ! Moving Reference Frame parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: bc_ns            ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structure

! -- OUTPUTS --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal Variables --
integer         :: ifb, if, ip, nf  ! index de liste, index de face limite et parametres
integer         :: ighost, ic, is   ! index de cellule interieure, et de cellule fictive
type(v3d)       :: pos
type(st_nsetat) :: nspri
real(krp), dimension(fct_buffer) :: mach, ps, pi, ti, s, x, y, z
type(v3d), dimension(fct_buffer) :: dir
integer                          :: ib, nblock, buf            ! block index and number of blocks
integer, pointer                 :: ista(:), iend(:)           ! starting and ending index
type(st_fct_env)                 :: env

! -- BODY --

if (unif /= uniform) call error_stop("Development: Condition non uniforme non implementee")

nf = ustboco%nface

call new_buf_index(nf, fct_buffer, nblock, ista, iend, nthread)

select case(bccon%bccon_mode)
! --- State BC --------------------------------------
case(bccon_cell_state, bccon_face_state)

call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

call new(nspri, nf)

call fct_env_set_real(blank_env, "t", curtime)

do ifb = 1, nf
  if  = ustboco%iface(ifb)
  ic  = bccon%isend(ifb)
  call fct_env_set_real(blank_env, "x", umesh%mesh%iface(if,1,1)%centre%x)
  call fct_env_set_real(blank_env, "y", umesh%mesh%iface(if,1,1)%centre%y)
  call fct_env_set_real(blank_env, "z", umesh%mesh%iface(if,1,1)%centre%z)
  call fct_eval_real(blank_env, bc_ns%ptot, pi(ifb))
  call fct_eval_real(blank_env, bc_ns%ttot, ti(ifb))
  call fct_eval_real(blank_env, bc_ns%mach, mach(ifb))
  call fct_eval_real(blank_env, bc_ns%dir_x, dir(ifb)%x)
  call fct_eval_real(blank_env, bc_ns%dir_y, dir(ifb)%y)
  call fct_eval_real(blank_env, bc_ns%dir_z, dir(ifb)%z)
  dir(ifb) = dir(ifb) / abs(dir(ifb)) ! moved from def_boco_ns.f90
enddo

call pi_ti_mach_dir2nspri(defns%properties(1), nf, pi, ti, mach, dir, nspri) 

! BOundary COnditions transformation in the Moving Reference Frame
if (mrf%type /= mrf_none .and. mrf%input == mrfdata_absolute) then
  do ifb = 1, nf
    if   = ustboco%iface(ifb)
    pos  = umesh%mesh%iface(if,1,1)%centre
    call mrfvel_abs2rel(mrf, curtime, pos, nspri%velocity(ifb))    ! DEV: need to vectorize ?
  enddo
endif

do ifb = 1, nf
  if     = ustboco%iface(ifb)
  ighost = bccon%irecv(ifb)
  bccon%frecv%tabscal(1)%scal(ighost) = nspri%density(ifb)
  bccon%frecv%tabscal(2)%scal(ighost) = nspri%pressure(ifb)
  bccon%frecv%tabvect(1)%vect(ighost) = nspri%velocity(ifb)
enddo

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL
call delete(nspri)

! --- Gradient BC --------------------------------------
case(bccon_cell_grad, bccon_face_grad)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, is, buf) &
!$OMP shared(ista, iend, nblock) 
!$OMP DO
do ib = 1, nblock
  do ifb = ista(ib), iend(ib)
    if = ustboco%iface(ifb)
    is = bccon%isend(ifb)
    ic = bccon%irecv(ifb)
    bccon%frecv%tabvect(1)%vect(ic) = bccon%fsend%tabvect(1)%vect(is) 
    bccon%frecv%tabvect(2)%vect(ic) = bccon%fsend%tabvect(2)%vect(is) 
    bccon%frecv%tabtens(1)%tens(ic) = bccon%fsend%tabtens(1)%tens(is) 
  enddo
enddo
!$OMP END DO
!$OMP END PARALLEL

! ------------------------------------------------------
case default
  call error_stop("Internal error: unknown connection mode (setboco_ns_inlet_sup)")
endselect

deallocate(ista, iend)

endsubroutine setboco_ns_inlet_sup
!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
! June 2008 : FCT function for pi, ti, mach (function of X, Y, Z)
! Mar  2010 : time dependent conditions
! Feb  2011 : symbolic funcions evaluation for DIRECTION fields (A. Gardi)
! Feb  2011 : Boundary Conditions transformation in the Moving Reference Frame
!------------------------------------------------------------------------------!
