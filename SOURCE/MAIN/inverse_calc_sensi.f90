!------------------------------------------------------------------------------!
! Procedure : inverse_calc_sensi
!
! Fonction
!
!------------------------------------------------------------------------------!
subroutine inverse_calc_sensi(lworld, inverse, exchcycle, ncoupling, flux, tref)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MENU_INVERSE

implicit none

! -- INPUTS --
type(mnu_inv)         :: inverse
integer               :: ncoupling     ! nombre de couplages
integer               :: exchcycle(*)  !
real(krp), intent(in) :: flux(inverse%nflux)                    ! ref. flux at "unknown" flux BOCO
real(krp), intent(in) :: tref(inverse%nmes, inverse%ncyc_futur) ! ref. temperature

! -- INPUTS/OUTPUTS --
type(st_world) :: lworld

! -- OUTPUTS --

! -- Internal variables --
integer   :: izone, ir
integer   :: ifut, nfut, im, nmode, im1, im2, imes
integer   :: nmes, nflux, izflux, ibflux, ibdefflux
real(krp) :: wcur_res
real(krp), allocatable :: tmes_calc(:,:)     ! computed tmes(1:nmes, 1:nfut) without flux
real(krp), allocatable :: unitmode(:)

! -- BODY --

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

nflux     = inverse%nflux
izflux    = inverse%iz_unknown
ibflux    = inverse%ib_unknown
ibdefflux = lworld%zone(izflux)%gridlist%first%umesh%boco(ibflux)%idefboco

!-------------------------------------------------------------------------------------
! computation of REFERENCE TEMPERATURE in future timesteps (with original fluxes) 
!-------------------------------------------------------------------------------------

nmode = inverse%defmode%nmode
nfut  = inverse%ncyc_futur
nmes  = lworld%prj%inverse%nmes

allocate(tmes_calc(nmes, nfut))
allocate(unitmode(1:nmode))

do im = 1, nmode

   ! -- RESTORE FIELD Qn --

   do izone = 1, lworld%prj%nzone
      call field_ref2cons(lworld%zone(izone)%gridlist%first%info%field_loc)
   enddo

   ! -- Apply Flux mode dQ --

   lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux) = flux(1:nflux)
    
   unitmode(1:nmode) = 0._krp
   unitmode(im)      = lworld%prj%inverse%ref_flux    ! define ( 0 ..., 1, 0 ... 0)

   call add_invmodes(inverse%defmode, &
                     lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux), &
                     unitmode)

   ! -- Compute nfut cycle to obtain tmes(dQ) --

   do ifut = 1, nfut

      do izone = 1, lworld%prj%nzone
         call integration_cyclezone(lworld%zone(izone), lworld%info%residu_ref, wcur_res)
      enddo
  
      call inverse_get_tmes(ifut, lworld%prj%inverse, lworld%zone(lworld%prj%inverse%iz_tmes), &
           tmes_calc)
   enddo

   lworld%prj%inverse%sensi(im, 1:nmes, 1:nfut) = (tmes_calc(1:nmes, 1:nfut) - tref(1:nmes, 1:nfut))&
                                                  / lworld%prj%inverse%ref_flux
   print*,"  mode",im," sensitivity:",maxval(abs(lworld%prj%inverse%sensi(im, 1:nmes, 1:nfut)))
   
enddo

! --- computation of MATRIX in (S^t * S).dQ = S^t * dT ---

do im1 = 1, nmode
  do im2 = 1, nmode
    lworld%prj%inverse%sensi2(im1, im2) = sum(  lworld%prj%inverse%sensi(im1,1:nmes,1:nfut) &
                                              * lworld%prj%inverse%sensi(im2,1:nmes,1:nfut) )
  enddo
enddo

!-----------------------------------------------
deallocate(tmes_calc)
deallocate(unitmode)


!-------------------------------------
endsubroutine inverse_calc_sensi

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008: created (from integration_cycle_inverse)
!------------------------------------------------------------------------------!
