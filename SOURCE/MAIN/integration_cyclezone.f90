!------------------------------------------------------------------------------!
! Procedure : integration_cyclezone       Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction 
!   Integration d'une zone sur un ecart de temps donne,
!   d'une representation physique uniquement
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_cyclezone(lzone, wres_ref, wcur_res)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D

implicit none

! -- Declaration des entrees --
type(st_zone) :: lzone            ! zone a integrer
real(krp)   :: wres_ref           ! world reference residual 
real(krp)   :: wcur_res           ! world current residual

! -- Declaration des sorties --

! -- Declaration des variables internes --
real(krp)   :: local_t            ! temps local (0 a mdt)
real(krp)   :: dt                 ! pas de temps de la zone
integer     :: if, ic, ib, nbc    ! index de champ, couplage et boco
real(krp)   :: part_cor           ! part de correction a appliquer
real(krp)   :: dtmax
integer     :: typ_cor            ! type de correction
logical     :: fin

!DEV
integer :: cumulreste, oui, non
! -- Debut de la procedure --

!DEV
oui = 1
non = 2
cumulreste = oui

lzone%info%iter_loc    = 0
lzone%info%cur_res     = lzone%info%residu_ref   ! defined or initialized in integration_cycle
local_t = 0._krp
fin     = .false.

! ecriture d'informations

select case(lzone%info%typ_temps)

case(stationnaire)
  write(str_w,'(a,i5)') "  zone",lzone%id

case(instationnaire)
  write(str_w,'(a,i5,a,g10.4)') "  zone",lzone%id," a t local =",local_t
  
case(periodique)

endselect

call print_info(7,str_w)

!----------------------------------
! integration loop on timesteps
!----------------------------------

do while (.not.fin)

  
  lzone%info%iter_loc = lzone%info%iter_loc + 1
  lzone%info%iter_tot = lzone%info%iter_tot + 1
  
  select case(lzone%info%typ_temps)
  case(stationnaire)
    dtmax = huge(dtmax)
  case(instationnaire)
    dtmax = lzone%info%cycle_dt - local_t
  case(periodique)
    call erreur("Developpement","cas non implemente")
  endselect

  !----------------------------------
  ! compute time step

  call calc_zonetimestep(lzone, dt, wres_ref, wcur_res, dtmax)

  !----------------------------------
  ! ecriture d'informations et gestion

  select case(lzone%info%typ_temps)
  case(stationnaire)
  case(instationnaire)
    if (dt >= (lzone%info%cycle_dt - local_t)) then
      fin = .true.
      dt  = lzone%info%cycle_dt - local_t
    endif  
  case(periodique)
    call erreur("Developpement","cas non implemente")
  endselect

  ! Correction de flux quand necessaire
  do ic = 1, lzone%ncoupling
    part_cor = lzone%coupling(ic)%partcor
    typ_cor = lzone%coupling(ic)%typ_cor
    if ( (typ_cor.ne.sans).and.(typ_cor.ne.auto).and.(typ_cor.ne.partiel).and.&
         (typ_cor.ne.bocoT).and.(typ_cor.ne.avant).and.(typ_cor.ne.apres).and.&
         (typ_cor.ne.bocoT2) ) then !DEV1603
      ! Calcul de l'indice de condition aux limites
      do ib = 1, lzone%gridlist%first%umesh%nboco
        if (samestring(lzone%coupling(ic)%family, &
                       lzone%gridlist%first%umesh%boco(ib)%family)) nbc = ib
      enddo
      ! Correction de flux
      if (cumulreste == oui) then
        call corr_varprim(lzone%gridlist%first%field, lzone%gridlist%first%umesh, &
                          lzone%defsolver, &
                          lzone%coupling(ic)%zcoupling%etatcons, nbc, &
                          part_cor, typ_cor, .false.)
      else
        call corr_varprim(lzone%gridlist%first%field, lzone%gridlist%first%umesh, &
                          lzone%defsolver, &
                          lzone%coupling(ic)%zcoupling%etatcons, nbc, &
                          part_cor, typ_cor, fin)
      endif 
    endif
  enddo

  !----------------------------------
  ! ecriture d'informations et gestion

  ! ---
  select case(lzone%defsolver%typ_solver)

  case(solKDIF, solNS)
    call integzone_tstep_usttree(dt, lzone)

  case(solVORTEX)
    call integration_zone_lag(dt, lzone)   !!! DEV !!! to be renamed "integzone_tstep_lag"
    lzone%info%residumax  = 2._krp      ! astuce pour n'imposer qu'une iteration 
    lzone%info%cur_res    = 1.e-8_krp   ! dans le cycle
    lzone%info%residu_ref = 1.e+8_krp   ! astuce pour n'avoir qu'un cycle

  case default
    call erreur("incoherence interne","solveur inattendu")
  endselect
  ! ---

  ! ecriture d'informations et test de fin de cycle

  select case(lzone%info%typ_temps)

  case(stationnaire)
    lzone%info%residu_ref = max(lzone%info%residu_ref, lzone%info%cur_res)
    if (lzone%info%cur_res/lzone%info%residu_ref <= lzone%info%residumax) fin = .true.
    if (mod(lzone%info%iter_loc,10) == 0) &
      write(str_w,'(a,i5,a,g10.4)') "    iteration",lzone%info%iter_loc," | residu = ", log10(lzone%info%cur_res)
!                                    log10(lzone%info%cur_res/lzone%info%residu_ref)

    !if (mod(lzone%info%iter_loc,10) == 0) call print_info(9,str_w)

  case(instationnaire)
    local_t = local_t + dt
    if (mod(lzone%info%iter_loc,10) == 0) &
!    if (fin) &
     write(str_w,'(a,i5,a,g10.4)') "    integration",lzone%info%iter_loc," a t local =",local_t
  
  case(periodique)

  endselect

   if (mod(lzone%info%iter_loc,10) == 0) call print_info(9,str_w)
!  if (fin) call print_info(9,str_w)

  call capteurs(lzone)

enddo

write(str_w,'(a,i5,a)') "    integration terminee en ",lzone%info%iter_loc," iterations"
call print_info(9,str_w)

!---------------------------------------
endsubroutine integration_cyclezone

!------------------------------------------------------------------------------!
! Change history
!
! juil 2002 : creation de la procedure
! juin 2003 : champs multiples
! juil 2003 : calcul du nombre de Fourier de la zone 
!             allocation des residus remontee a integration_macrodt
! sept 2003 : calcul des gradients
! oct  2003 : deplacement des proc. calc_gradient et calc_varprim dans integration_zone
! mars 2004 : integration de zone par technique lagrangienne
! oct  2004 : field chained list
! mar  2006 : integrationmacro_zone changed to integration_cyclezone
!             update_champ moved to integzone_tstep_usttree (new name of integration_zone)
!------------------------------------------------------------------------------!

