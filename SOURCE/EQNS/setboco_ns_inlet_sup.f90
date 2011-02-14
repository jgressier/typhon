!------------------------------------------------------------------------------!
! Procedure : setboco_ns_inlet_sup        Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction    
!   Computation of supersonic inlet boundary conditions
!   
!------------------------------------------------------------------------------!
subroutine setboco_ns_inlet_sup(curtime, defns, mrf, unif, bc_ns, ustboco, umesh, fld)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD 
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
type(st_field)   :: fld              ! fld des etats

! -- Internal Variables --
integer         :: ifb, if, ip, nf  ! index de liste, index de face limite et parametres
integer         :: ighost, ic       ! index de cellule interieure, et de cellule fictive
type(v3d)       :: pos
type(st_nsetat) :: nspri
real(krp), allocatable :: mach(:), pi(:), ti(:)
type(v3d), allocatable :: dir(:)

! -- BODY --

if (unif /= uniform) call error_stop("Development: Condition non uniforme non implementee")

nf = ustboco%nface

call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

call new(nspri, nf)
allocate(mach(nf))
allocate(pi(nf))
allocate(ti(nf))
allocate(dir(nf))


call fct_env_set_real(blank_env, "t", curtime)

do ifb = 1, nf
  if  = ustboco%iface(ifb)
  ic  = umesh%facecell%fils(if,1)
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
if (mrf%type /= mrf_none) then
  do ifb = 1, nf
    if   = ustboco%iface(ifb)
    pos  = umesh%mesh%iface(if,1,1)%centre
    call mrf_abs2rel(mrf, curtime, pos, nspri%velocity(ifb))    ! DEV: need to vectorize ?
  enddo
endif

do ifb = 1, nf
  if     = ustboco%iface(ifb)
  ighost = umesh%facecell%fils(if,2)
  fld%etatprim%tabscal(1)%scal(ighost) = nspri%density(ifb)
  fld%etatprim%tabscal(2)%scal(ighost) = nspri%pressure(ifb)
  fld%etatprim%tabvect(1)%vect(ighost) = nspri%velocity(ifb)
enddo

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL
call delete(nspri)
deallocate(pi, ti, mach, dir)

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
