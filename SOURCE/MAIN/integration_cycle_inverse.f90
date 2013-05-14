!------------------------------------------------------------------------------!
! Procedure : integration_cycle_inverse
!
! Fonction
!   Integration de cycle pour toutes les zones
!   Gestion des communications et couplages entre zones
!   pour un calcul inverse uniquement
!
!------------------------------------------------------------------------------!
subroutine integration_cycle_inverse(lworld, exchcycle, ncoupling)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MENU_INVERSE
use MATRIX

implicit none

! -- INPUTS --
integer                         :: ncoupling  ! nombre de couplages
integer, dimension(1:ncoupling) :: exchcycle  ! indice du cycle d'echange
                                              ! (pour les differents couplages de zones)

! -- INPUTS/OUTPUTS --
type(st_world) :: lworld

! -- OUTPUTS --

! -- Internal variables --
integer   :: izone
integer   :: ifut, ic, im, izflux, ibflux, ibdefflux
integer   :: nmode, nmes, nfut, nflux
real(krp) :: wcur_res
real(krp), allocatable :: tmes_ref(:,:)     ! computed tmes(1:nmes, 1:nfut) without flux
real(krp), allocatable :: mat(:,:), rhs(:)  ! algebric problem
real(krp), allocatable :: flux(:)           ! flux to save

! -- BODY --

! verification calcul inverse


! -- CHECK parameters

if (ncoupling > 0) then
  call erreur("Internal error (integration_cycle_inverse)","unable to use coupled zones when using inverse mode")
endif

if (lworld%prj%time_model /= time_unsteady_inverse) then
  call erreur("Internal error (integration_cycle_inverse)","ONLY unsteady inverse mode")
endif

if (lworld%prj%nzone /= 1) then
  call erreur("Internal error (integration_cycle_inverse)","ONLY ONE zone allowed")
endif

nmode = lworld%prj%inverse%defmode%nmode
nfut  = lworld%prj%inverse%ncyc_futur
nmes  = lworld%prj%inverse%nmes
nflux = lworld%prj%inverse%nflux

write(str_w,'(a,g11.4)') "  inverse computation at time =",lworld%info%curtps
call print_info(7,str_w)

!-------------------------------------------------------------------------------------
! UPDATE TMES FIELD (shift and read)

do ifut = 1, nfut-1
   lworld%prj%inverse%tmes_expe(:,ifut) = lworld%prj%inverse%tmes_expe(:,ifut+1)
enddo

! -- Lecture des Tmes au temps t+nfutur --

read(lworld%prj%inverse%tmes_funit,*) (lworld%prj%inverse%tmes_expe(ic,nfut), ic=1,nmes)

!-------------------------------------------------------------------------------------
! SAVE Reference time Qn and flux conditions
!-------------------------------------------------------------------------------------

! -- SAVE FIELD Qn --

do izone = 1, lworld%prj%nzone
  call field_cons2ref(lworld%zone(izone)%gridlist%first%info%field_loc)
enddo

! -- save flux --

allocate(flux(nflux))

izflux    = lworld%prj%inverse%iz_unknown
ibflux    = lworld%prj%inverse%ib_unknown
ibdefflux = lworld%zone(izflux)%gridlist%first%umesh%boco(ibflux)%idefboco

flux(1:nflux) = lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux)

!-------------------------------------------------------------------------------------
! computation of REFERENCE TEMPERATURE in future timesteps (with original fluxes) 
!-------------------------------------------------------------------------------------

allocate(tmes_ref(nmes, nfut))

call print_info(10, ". prospecting ("//trim(strof(nfut))//" cycles)")

do ifut = 1, nfut

  do izone = 1, lworld%prj%nzone
    lworld%zone(izone)%info%cycle_dt = lworld%prj%dtbase
    call integration_cyclezone(lworld%zone(izone), lworld%info%residu_ref, wcur_res)
  enddo

  call inverse_get_tmes(lworld%info, ifut, lworld%prj%inverse, lworld%zone(lworld%prj%inverse%iz_tmes), &
                        tmes_ref)
enddo

!------------------------------------------------
! computation of sensivity matrix if necessary

if (mod(lworld%info%icycle-1, lworld%prj%inverse%ncyc_sensi) == 0) then
  call print_info(10, ". sensitivity computation")
  call inverse_calc_sensi(lworld, lworld%prj%inverse, exchcycle, ncoupling, flux, tmes_ref)
endif

!-------------------------------------------------------------------------------------
! RESTORE Reference time Qn and flux conditions

lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux) = flux(1:nflux)

! -- RESTORE FIELD Qn --

do izone = 1, lworld%prj%nzone
  call field_ref2cons(lworld%zone(izone)%gridlist%first%info%field_loc)
enddo

!------------------------------------------------
! Solve LSQ problem to obtain magnitude of FLUX DCT MODES

allocate(rhs(nmode))
allocate(mat(nmode,nmode))

! --- computation of RHS in (S^t * S).dQ = S^t * dT ---

do im = 1, nmode
  rhs(im) = sum( (lworld%prj%inverse%tmes_expe(1:nmes,1:nfut)-tmes_ref(1:nmes,1:nfut))  &
                 *lworld%prj%inverse%sensi(im,1:nmes,1:nfut) )
enddo
mat(:,:)=lworld%prj%inverse%sensi2(:,:)

! --- SOLVE ---

call cholesky_decomp(mat, nmode)
call cholesky_solve (mat, nmode, rhs, 1)

call add_invmodes(lworld%prj%inverse%defmode, &
                  lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux), &
                  rhs)

!-----------------------------------------------
!---- integration d'1 cycle : calcul à t+1 -----
!-----------------------------------------------

call print_info(10, ". actual cycle")

do izone = 1, lworld%prj%nzone
 
  ! -- Initialisation des infos pour le cycle
  !lworld%zone(izone)%info%iter_tot  = 0
  !lworld%zone(izone)%info%typ_temps = lworld%prj%typ_temps
  lworld%zone(izone)%info%cycle_dt = lworld%prj%dtbase

  !-------------------------------------
  call integration_cyclezone(lworld%zone(izone), lworld%info%residu_ref, wcur_res)
  !-------------------------------------

  ! -- Initialisation de residu_reforigine : valeur du residu de reference du premier cycle
  if(lworld%info%icycle.eq.1) then
    lworld%zone(izone)%info%residu_reforigine = lworld%zone(izone)%info%residu_ref
    open(unit=601, file="inverse_flux.dat", form="formatted")
  endif
  write(601,*) lworld%zone(izflux)%defsolver%boco(ibdefflux)%boco_kdif%flux_nunif(1:nflux)

  ! -- Retour d'informations d'integration du cycle
  lworld%zone(izone)%info%time_model = lworld%prj%time_model
  lworld%zone(izone)%info%cycle_dt   = lworld%prj%dtbase
enddo

!-----------------------------------------------
deallocate(mat, rhs)
deallocate(tmes_ref, flux)

!-------------------------------------
endsubroutine integration_cycle_inverse

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008: created (from integration_cycle)
!------------------------------------------------------------------------------!
