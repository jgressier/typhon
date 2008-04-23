!------------------------------------------------------------------------------!
! Procedure : inverse_calc_sensi
!
! Fonction
!
!------------------------------------------------------------------------------!
subroutine inverse_calc_sensi(lworld, exchcycle, ncoupling)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MENU_INVERSE

implicit none

! -- INPUTS --
integer                         :: ncoupling  ! nombre de couplages
integer, dimension(1:ncoupling) :: exchcycle  ! indice du cycle d'echange
                                              ! (pour les differents couplages de zones)

! -- INPUTS/OUTPUTS --
type(st_world) :: lworld

! -- OUTPUTS --

! -- Internal variables --
integer   :: izone, ir
integer   :: ifut, im
real(krp) :: wcur_res
real(krp), allocatable :: tmes_calc(:,:)     ! computed tmes(1:nmes, 1:nfut) without flux

! -- BODY --

! verification calcul inverse


! -- CHECK parameters

if (ncoupling > 0) then
  call erreur("Internal error (inverse_calc_sensi)","unable to use coupled zones when using inverse mode")
endif

if (lworld%prj%time_model /= time_unsteady_inverse) then
  call erreur("Internal error (inverse_calc_sensi)","ONLY unsteady inverse mode")
endif

if (lworld%prj%nzone /= 1) then
  call erreur("Internal error (inverse_calc_sensi)","ONLY ONE zone allowed")
endif


!-------------------------------------------------------------------------------------
! computation of REFERENCE TEMPERATURE in future timesteps (with original fluxes) 
!-------------------------------------------------------------------------------------

allocate(tmes_calc(lworld%prj%inverse%nmes, lworld%prj%inverse%ncyc_futur))

do im = 1, lworld%prj%inverse%ndctmode

   ! -- RESTORE FIELD Qn --

   do izone = 1, lworld%prj%nzone
      call field_ref2cons(lworld%zone(izone)%gridlist%first%info%field_loc)
   enddo

   ! -- Apply Flux mode dQ --


   ! -- Compute nfut cycle to obtain tmes(dQ) --

   do ifut = 1, lworld%prj%inverse%ncyc_futur

      do izone = 1, lworld%prj%nzone
         call integration_cyclezone(lworld%zone(izone), lworld%info%residu_ref, wcur_res)
      enddo
  
      call inverse_get_tmes(ifut, lworld%prj%inverse, lworld%zone(lworld%prj%inverse%iz_tmes), &
           tmes_calc)

   enddo

enddo

!-----------------------------------------------
deallocate(tmes_calc)


!-------------------------------------
endsubroutine inverse_calc_sensi

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008: created (from integration_cycle_inverse)
!------------------------------------------------------------------------------!
