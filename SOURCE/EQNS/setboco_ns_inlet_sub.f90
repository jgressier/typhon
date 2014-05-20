!------------------------------------------------------------------------------!
! Procedure : setboco_ns_inlet_sub        Auteur : J. Gressier
!                                         Date   : July 2004
!> @brief Computation of supersonic inlet boundary conditions
!------------------------------------------------------------------------------!
subroutine setboco_ns_inlet_sub(defsolver, unif, boco_ns, ustboco, umesh, bccon, curtime)

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
use PACKET

implicit none

! -- INPUTS --
real(krp)        :: curtime
type(mnu_solver) :: defsolver            ! solver parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: boco_ns            ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structure

! -- OUTPUTS --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer                :: ifb, if, ip, nf, ic, is, iloc  ! index de liste, index de face limite et parametres
real(krp), dimension(fct_buffer) :: ps, pi, ti, s, x, y, z
type(v3d), dimension(fct_buffer) :: dir
integer                          :: ib, nblock, buf            ! block index and number of blocks
integer, pointer                 :: ista(:), iend(:)           ! starting and ending index
type(st_fct_env)                 :: env
logical                          :: xyz_depend
type(st_nsetat)                  :: nspri
type(v3d)              :: pos
real(krp)              :: gam, gsgmu

! -- BODY --

if (unif /= uniform) call error_stop("Development: non uniform condition not implemented")

nf = ustboco%nface

call new_buf_index(nf, fct_buffer, nblock, ista, iend, nthread)

gam   = defsolver%defns%properties(1)%gamma
gsgmu = gam/(gam-1._krp)

select case(bccon%bccon_mode)
! --- State BC --------------------------------------
case(bccon_cell_state, bccon_face_state)

!$OMP PARALLEL & 
!$OMP private(ifb, if, ic, ib, env, x, y, z, pi, ps, ti, s, buf, nspri, pos, dir) &
!$OMP shared(ista, iend, nblock, curtime) 
  
call new_fct_env(env)      ! temporary environment from FCT_EVAL
call fct_env_set_real(env, "t", curtime)
call new(nspri, fct_buffer)

!$OMP DO
block: do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  if (boco_ns%xyz_depend) then
    do ifb = 1, buf
      if   = ustboco%iface(ista(ib)+ifb-1)
      x(ifb) = umesh%mesh%face_center(if,1)%x
      y(ifb) = umesh%mesh%face_center(if,1)%y
      z(ifb) = umesh%mesh%face_center(if,1)%z
    enddo

    call fct_env_set_realarray(env, "x", x(1:buf))
    call fct_env_set_realarray(env, "y", y(1:buf))
    call fct_env_set_realarray(env, "z", z(1:buf))
  endif

  do ifb = 1, buf
    if   = ustboco%iface(ista(ib)+ifb-1)
    ic   = bccon%isend(ifb)
    ps(ifb) = bccon%fsend%tabscal(2)%scal(ic)
  enddo
  
  call fct_eval_realarray(env, boco_ns%ptot, pi(1:buf))
  if (boco_ns%is_ttot) then
    call fct_eval_realarray(env, boco_ns%ttot, ti(1:buf))
  else
    call fct_eval_realarray(env, boco_ns%entropy, s(1:buf))  !  "s" = p/(rho**gamma) = p**(1-gamma)*(rT)**gamma
    ti(1:buf) = (s(1:buf)*pi(1:buf)**(gam-1._krp))**(1._krp/gam) / defsolver%defns%properties(1)%r_const
  endif
  
  call fct_eval_realarray(env, boco_ns%dir_x, x(1:buf))
  call fct_eval_realarray(env, boco_ns%dir_y, y(1:buf))
  call fct_eval_realarray(env, boco_ns%dir_z, z(1:buf))
  
  do ifb = 1, buf
    dir(ifb)%x = x(ifb)
    dir(ifb)%y = y(ifb)
    dir(ifb)%z = z(ifb)
  enddo
  dir(1:buf) = dir(1:buf) / abs(dir(1:buf)) ! moved from def_boco_ns.f90

  call pi_ti_ps_dir2nspri(defsolver%defns%properties(1), buf, pi(1:buf), ti(1:buf), ps(1:buf), dir(1:buf), &
                          nspri) 

  ! BOundary COnditions transformation in the Moving Reference Frame
  if (defsolver%defmrf%type /= mrf_none .and. defsolver%defmrf%input == mrfdata_absolute) then
    do ifb = 1, buf
      if   = ustboco%iface(ista(ib)+ifb-1)
      pos  = umesh%mesh%face_center(if,1)
      call mrfvel_abs2rel(defsolver%defmrf, curtime, pos, nspri%velocity(ifb))    ! DEV: need to vectorize ?
    enddo
  endif

  do iloc = 1, buf
    ifb = ista(ib)+iloc-1
    ic = bccon%irecv(ifb)
    bccon%frecv%tabscal(1)%scal(ic) = nspri%density(iloc)
    bccon%frecv%tabscal(2)%scal(ic) = nspri%pressure(iloc)
    bccon%frecv%tabvect(1)%vect(ic) = nspri%velocity(iloc)
  enddo

enddo block
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
  do ifb = ista(ib), iend(ifb)
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
  call error_stop("Internal error: unknown connection mode (setboco_ns_inlet_sub)")
endselect

deallocate(ista, iend)

endsubroutine setboco_ns_inlet_sub
!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
! June 2008 : FCT function for pi, ti, mach (function of X, Y, Z)
! Mar  2010 : time dependent conditions
! Feb  2011 : symbolic functions evaluation for DIRECTION fields (A. Gardi)
! Feb  2011 : Boundary Conditions transformation in the Moving Reference Frame
! Jun  2013 : (optional) entropy based computation, vectorization of FCT, and OMP parallelization
!------------------------------------------------------------------------------!
