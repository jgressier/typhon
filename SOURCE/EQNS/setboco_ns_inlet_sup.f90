!------------------------------------------------------------------------------!
! Procedure : setboco_ns_inlet_sup        Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction    
!   Computation of supersonic inlet boundary conditions
!   
!------------------------------------------------------------------------------!
subroutine setboco_ns_inlet_sup(defsolver, unif, boco_ns, ustboco, umesh, bccon, curtime)

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
type(mnu_solver) :: defsolver        ! solver parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: boco_ns          ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structure

! -- OUTPUTS --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal Variables --
integer         :: ifb, if, ip, nf, iloc  ! index de liste, index de face limite et parametres
integer         :: ighost, ic, is   ! index de cellule interieure, et de cellule fictive
type(v3d)       :: pos
type(st_nsetat) :: nspri
real(krp), dimension(fct_buffer) :: mach, pi, ti, x, y, z
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

call fctset_initdependency(defsolver%fctenv)
call fctset_checkdependency(defsolver%fctenv, boco_ns%ptot)
call fctset_checkdependency(defsolver%fctenv, boco_ns%ttot)
call fctset_checkdependency(defsolver%fctenv, boco_ns%mach)
call fctset_checkdependency(defsolver%fctenv, boco_ns%dir_x)
call fctset_checkdependency(defsolver%fctenv, boco_ns%dir_y)
call fctset_checkdependency(defsolver%fctenv, boco_ns%dir_z)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, env, x, y, z, pi, mach, ti, buf, nspri, pos, dir, iloc, ighost) &
!$OMP shared(ista, iend, nblock, curtime) 
  
call new_fct_env(env)      ! temporary environment from FCT_EVAL
call fct_env_set_real(env, "t", curtime)
call new(nspri, fct_buffer)

!$OMP DO
do ib = 1, nblock
  buf = iend(ib)-ista(ib)+1
  
  if (boco_ns%xyz_depend) then
    do iloc = 1, buf
      ifb = iloc+ista(ib)-1
      if  = ustboco%iface(ifb)
      x(iloc) = umesh%mesh%iface(if,1,1)%centre%x
      y(iloc) = umesh%mesh%iface(if,1,1)%centre%y
      z(iloc) = umesh%mesh%iface(if,1,1)%centre%z
    enddo
    call fct_env_set_realarray(env, "x", x(1:buf))
    call fct_env_set_realarray(env, "y", y(1:buf))
    call fct_env_set_realarray(env, "z", z(1:buf))
  endif

  call fctset_compute_neededenv(defsolver%fctenv, env)

  call fct_eval_realarray(env, boco_ns%ptot, pi)
  call fct_eval_realarray(env, boco_ns%ttot, ti)
  call fct_eval_realarray(env, boco_ns%mach, mach)
  call fct_eval_realarray(env, boco_ns%dir_x, x)
  call fct_eval_realarray(env, boco_ns%dir_y, y)
  call fct_eval_realarray(env, boco_ns%dir_z, z)
  do iloc = 1, buf
    dir(iloc)%x = x(iloc)
    dir(iloc)%y = y(iloc)
    dir(iloc)%z = z(iloc)
  enddo
  dir(1:buf) = dir(1:buf) / abs(dir(1:buf)) ! moved from def_boco_ns.f90

  call pi_ti_mach_dir2nspri(defsolver%defns%properties(1), buf, pi, ti, mach, dir, nspri) 

  ! BOundary COnditions transformation in the Moving Reference Frame
  if (defsolver%defmrf%type /= mrf_none .and. defsolver%defmrf%input == mrfdata_absolute) then
    do iloc = 1, buf
      ifb  = iloc+ista(ib)-1
      if   = ustboco%iface(ifb)
      pos  = umesh%mesh%iface(if,1,1)%centre
      call mrfvel_abs2rel(defsolver%defmrf, curtime, pos, nspri%velocity(iloc))    ! DEV: need to vectorize ?
    enddo
  endif

  do iloc = 1, buf
    ifb = iloc+ista(ib)-1
    if  = ustboco%iface(ifb)
    ighost = bccon%irecv(ifb)
    bccon%frecv%tabscal(1)%scal(ighost) = nspri%density(iloc)
    bccon%frecv%tabscal(2)%scal(ighost) = nspri%pressure(iloc)
    bccon%frecv%tabvect(1)%vect(ighost) = nspri%velocity(iloc)
  enddo

enddo ! block loop
!$OMP END DO
call delete_fct_env(env)      ! temporary environment from FCT_EVAL
call delete(nspri)
!$OMP END PARALLEL

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
