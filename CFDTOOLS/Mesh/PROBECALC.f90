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

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
subroutine prb_vol_init(probe)
implicit none
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe
! -- BODY --
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
! Routine: prb_vol   
! Computation of volumic probe
!------------------------------------------------------------------------------!
subroutine prb_vol_calc(probe, umesh, field)
implicit none
! -- INPUTS --
type(st_ustmesh)         :: umesh
type(st_genericfield)    :: field
! -- INPUTS/OUTPUTS --
type(st_defprobe)        :: probe
! -- Internal variables --
integer(kip)             :: ic, isca, ivec
real(krp)                :: result
character(len=shortname) :: qname

! -- BODY --

call new_fct_env(blank_env)      ! temporary environment from FCT_EVAL

do ic = 1, umesh%ncell_int

  call fct_env_set_real(blank_env, "x", umesh%mesh%centre(ic,1,1)%x)
  call fct_env_set_real(blank_env, "y", umesh%mesh%centre(ic,1,1)%y)
  call fct_env_set_real(blank_env, "z", umesh%mesh%centre(ic,1,1)%z)
 
  do isca = 1, field%nscal
    qname = quantity_name(field%tabscal(isca)%quantity_id)
    call fct_env_set_real(blank_env, trim(qname), field%tabscal(isca)%scal(ic))
  enddo
  do ivec = 1, field%nvect
    qname = quantity_name(field%tabvect(ivec)%quantity_id)
    call fct_env_set_real(blank_env, trim(qname), abs(field%tabvect(ivec)%vect(ic)))
    call fct_env_set_real(blank_env, trim(qname)//"_X", field%tabvect(ivec)%vect(ic)%x)
    call fct_env_set_real(blank_env, trim(qname)//"_Y", field%tabvect(ivec)%vect(ic)%y)
    call fct_env_set_real(blank_env, trim(qname)//"_Z", field%tabvect(ivec)%vect(ic)%z)
  enddo

  !call print_fct_env(6, blank_env)
  call fct_eval_real(blank_env, probe%quantity, result)

  select case(probe%type)
  case(vol_min)
    probe%result = min(probe%result, result)
  case(vol_max)
    probe%result = max(probe%result, result)
  case(vol_average)
    probe%result = probe%result + result*umesh%mesh%volume(ic,1,1)
    probe%volume = probe%volume + umesh%mesh%volume(ic,1,1)
  case default
    call cfd_error("Internal error (prb_vol): unknown probe type")
  endselect
  
enddo 

select case(probe%type)
case(vol_min, vol_max)
  ! nothing to do
case(vol_average)
  probe%result = probe%result / probe%volume
case default
  call cfd_error("Internal error (prb_vol): unknown probe type")
endselect

call delete_fct_env(blank_env)      ! temporary environment from FCT_EVAL

endsubroutine prb_vol_calc

endmodule PROBECALC
!------------------------------------------------------------------------------!
! Changes history
!
! June 2011: created from SOURCE/MGRID/prb_grid_vol.f90
!------------------------------------------------------------------------------!




