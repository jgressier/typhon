!------------------------------------------------------------------------------!
! MODULE : PROBECALC                       Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition des capteurs
!
!------------------------------------------------------------------------------!
module PROBECALC

use MESHPREC
use DEFPROBE
use USTMESH
use GENFIELD
use QUANTITY
use FCT_EVAL
use FCT_ENV
use PACKET
!$ use OMP_LIB

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
subroutine prb_vol_init(probe, probetype)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe
integer(kpp), optional   :: probetype
! -- BODY --

if (present(probetype)) probe%type = probetype
select case(probe%type)
case(vol_min)
  probe%result = huge(probe%result)
case(vol_max)
  probe%result = -huge(probe%result)
case(vol_average)
  probe%result = 0._krp
  probe%volume = 0._krp
case default
  call cfd_error("Internal error (prb_vol_init): unknown probe type")
endselect

endsubroutine prb_vol_init

!------------------------------------------------------------------------------!
subroutine prb_vol_add(probe, result, vol)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe
real(krp), dimension(:)  :: result, vol
integer   :: siz
real(krp) :: pvol
! -- BODY --

siz = size(result)

select case(probe%type)
case(vol_min)
  probe%result = min(probe%result, minval(result(1:siz)))
case(vol_max)
  probe%result = max(probe%result, maxval(result(1:siz)))
case(vol_average)
  pvol = sum(vol(1:siz))
  probe%result = (probe%result*probe%volume + sum(result(1:siz)*vol(1:siz)))/(probe%volume+pvol)
  probe%volume = probe%volume + pvol
case default
  call cfd_error("Internal error (prb_vol_add): unknown probe type")
endselect

endsubroutine prb_vol_add

!------------------------------------------------------------------------------!
subroutine prb_vol_addprb(probe, prb)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe, prb(:)
integer   :: i, siz
real(krp) :: vol
! -- BODY --

siz = size(prb)

select case(probe%type)
case(vol_min)
  do i = 1, siz
    probe%result = min(probe%result, prb(i)%result)
  enddo
case(vol_max)
  do i = 1, siz
    probe%result = max(probe%result, prb(i)%result)
  enddo
case(vol_average)
  probe%result = probe%result * probe%volume
  do i = 1, siz
    probe%result = probe%result + prb(i)%result*prb(i)%volume
    probe%volume = probe%volume + prb(i)%volume
  enddo
  probe%result = probe%result / probe%volume
case default
  call cfd_error("Internal error (prb_vol_addprb): unknown probe type")
endselect

endsubroutine prb_vol_addprb

!------------------------------------------------------------------------------!
! Routine: prb_vol
! Computation of volumic probe
!------------------------------------------------------------------------------!
subroutine prb_vol_calc(curtime, probe, umesh, field)
implicit none
! -- INPUTS --
real(krp)                :: curtime
type(st_ustmesh)         :: umesh
type(st_genericfield)    :: field
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe
! -- Internal variables --
integer(kip)             :: ic, isca, ivec, il !! il should be removed ??
integer(kip)             :: nblock, nthread, ithread, ib, buf
character(len=shortname) :: qname
type(st_fct_env)         :: env
real(krp), dimension(fct_buffer) :: x, y, z, result
type(st_defprobe), allocatable   :: probeloc(:)
integer, pointer         :: ista(:), iend(:) ! starting and ending index
logical                  :: xyz_depend, qsca_depend(field%nscal), qvec_depend(field%nvect)

! -- BODY --

call new_buf_index(umesh%ncell_int, fct_buffer, nblock, ista, iend)

!call fctset_initdependency(defsolver%fctenv)
!call fctset_checkdependency(defsolver%fctenv, probe%quantity)

xyz_depend = fct_dependency(probe%quantity, "x").or. &
             fct_dependency(probe%quantity, "y").or. &
             fct_dependency(probe%quantity, "z")

!xyz_depend = xyz_depend .or. &
!               fctset_needed_dependency(defsolver%fctenv, "x").or. &
!               fctset_needed_dependency(defsolver%fctenv, "y").or. &
!               fctset_needed_dependency(defsolver%fctenv, "z")

do isca = 1, field%nscal
  qname = quantity_name(field%tabscal(isca)%quantity_id)
  qsca_depend(isca) = fct_dependency(probe%quantity, trim(qname)) ! .or.&
  !                   fctset_needed_dependency(fctenv, trim(qname))
enddo
do ivec = 1, field%nvect
  qname = quantity_name(field%tabvect(ivec)%quantity_id)
  qvec_depend(ivec) = fct_dependency(probe%quantity, trim(qname)) .or.&
                      fct_dependency(probe%quantity, trim(qname)//"_x") .or.&
                      fct_dependency(probe%quantity, trim(qname)//"_y") .or.&
                      fct_dependency(probe%quantity, trim(qname)//"_z") !.or.&
  !                   fctset_needed_dependency(fctenv, trim(qname)) ...
enddo

nthread = 1
!$OMP PARALLEL
!$ nthread = OMP_GET_NUM_THREADS()
!$OMP END PARALLEL
allocate(probeloc(nthread))

!$OMP PARALLEL &
! should be changed to : ??
!!$OMP private(ic, isca, ivec, result, env, x, y, z, buf, ithread, qname) &
!$OMP private(ic, il, result, env, x, y, z, buf, ithread, qname) &
!$OMP shared(ista, iend, nblock, curtime, xyz_depend, qsca_depend, qvec_depend, probe, probeloc)

call new_fct_env(env)      ! temporary environment from FCT_EVAL
call fct_env_set_real(env, "t", curtime)

ithread = 1
!$ ithread = OMP_GET_THREAD_NUM()+1
call prb_vol_init(probeloc(ithread), probe%type)

!$OMP DO
block: do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  if (xyz_depend) then
    do ic = ista(ib), iend(ib)
      x(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%x
      y(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%y
      z(ic-ista(ib)+1) = umesh%mesh%centre(ic,1,1)%z
    enddo

    call fct_env_set_realarray(env, "x", x(1:buf))
    call fct_env_set_realarray(env, "y", y(1:buf))
    call fct_env_set_realarray(env, "z", z(1:buf))
  endif

  !call fctset_compute_neededenv(defsolver%fctenv, env)

  do isca = 1, field%nscal
    if (qsca_depend(isca)) then
      qname = quantity_name(field%tabscal(isca)%quantity_id)
      call fct_env_set_realarray(env, trim(qname), field%tabscal(isca)%scal(ista(ib):iend(ib)))
    endif
  enddo
  do ivec = 1, field%nvect
    if (qvec_depend(ivec)) then
      qname = quantity_name(field%tabvect(ivec)%quantity_id)
      call fct_env_set_realarray(env, trim(qname), abs(field%tabvect(ivec)%vect(ista(ib):iend(ib))))
      do ic = ista(ib), iend(ib)
        x(ic-ista(ib)+1) = field%tabvect(ivec)%vect(ic)%x
        y(ic-ista(ib)+1) = field%tabvect(ivec)%vect(ic)%y
        z(ic-ista(ib)+1) = field%tabvect(ivec)%vect(ic)%z
      enddo
      call fct_env_set_realarray(env, trim(qname)//"_x", x(1:buf))
      call fct_env_set_realarray(env, trim(qname)//"_y", y(1:buf))
      call fct_env_set_realarray(env, trim(qname)//"_z", z(1:buf))
    endif
  enddo

  call fct_eval_realarray(env, probe%quantity, result)

  call prb_vol_add(probeloc(ithread), result(1:buf), umesh%mesh%volume(ista(ib):iend(ib),1,1))

enddo block

!$OMP END DO
call delete_fct_env(env)      ! temporary environment from FCT_EVAL
!$OMP END PARALLEL

! end of probe processing

call prb_vol_addprb(probe, probeloc(1:nthread))

deallocate(probeloc)

endsubroutine prb_vol_calc

endmodule PROBECALC
!------------------------------------------------------------------------------!
! Changes history
!
! Jun 2011: created from SOURCE/MGRID/prb_grid_vol.f90
! Mar 2013: vectorization & multithreading openmp
!------------------------------------------------------------------------------!

