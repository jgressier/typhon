!------------------------------------------------------------------------------!
! Procedure : integrationmacro_zone       Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction 
!   Integration d'une zone sur un ecart de temps donne,
!   d'une representation physique uniquement
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integrationmacro_zone(lzone, wres_ref, wcur_res)
 
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
real(krp)   :: dt                 ! pas de temps de la zone
integer     :: if, ic, ib, nbc    ! index de champ, couplage et boco
real(krp)   :: part_cor           ! part de correction a appliquer
real(krp)   :: dtmax
integer     :: typ_cor            ! type de correction

!DEV
integer :: cumulreste, oui, non
! -- Debut de la procedure --

!DEV
oui = 1
non = 2
cumulreste = oui

lzone%info%iter_loc    = 0
lzone%info%cur_res     = lzone%info%residu_ref   ! defined or initialized in integration_cycle
lzone%info%cycle_time = 0._krp
lzone%info%end_cycle   = .false.

! ecriture d'informations

select case(lzone%info%typ_temps)

case(stationnaire)
  write(str_w,'(a,i5)') "  zone",lzone%id

case(instationnaire)
  write(str_w,'(a,i5,a,g10.4)') "  zone",lzone%id," a t local =",lzone%info%cycle_time
  
case(periodique)

endselect

call print_info(7,str_w)

!----------------------------------
! integration
!----------------------------------

do while (.not.lzone%info%end_cycle)

  
  lzone%info%iter_loc = lzone%info%iter_loc + 1
  lzone%info%iter_tot = lzone%info%iter_tot + 1
  
  select case(lzone%info%typ_temps)
  case(stationnaire)
    dtmax = huge(dtmax)
  case(instationnaire)
    dtmax = lzone%info%cycle_dt - lzone%info%cycle_time
  case(periodique)
    call erreur("Development","periodic case not implemented")
  endselect

  ! ---
  call calc_zonetimestep(lzone, dt, wres_ref, wcur_res, dtmax)
  ! ---

  ! ecriture d'informations et gestion

  select case(lzone%info%typ_temps)
  case(stationnaire)
  case(instationnaire)
    if (dt >= (lzone%info%cycle_dt - lzone%info%cycle_time)) then
      lzone%info%end_cycle = .true.
      dt  = lzone%info%cycle_dt - lzone%info%cycle_time
    endif  
  case(periodique)
    call erreur("Development","periodic case not implemented")
  endselect

  ! Correction de flux quand necessaire

  ! PROVISOIRE A AMELIORER : correction seulement si KDIF
  if (lzone%defsolver%typ_solver == solKDIF) then
    do ic = 1, lzone%ncoupling
      part_cor = lzone%coupling(ic)%partcor
      typ_cor = lzone%coupling(ic)%typ_cor
      if ( (typ_cor.ne.sans).and.(typ_cor.ne.auto).and.(typ_cor.ne.partiel).and.&
         (typ_cor.ne.bocoT).and.(typ_cor.ne.avant).and.(typ_cor.ne.apres).and.&
         (typ_cor.ne.bocoT2).and.(typ_cor.ne.distributed)) then !DEV1603
      ! Calcul de l'indice de condition aux limites
      do ib = 1, lzone%grid%umesh%nboco
        if (samestring(lzone%coupling(ic)%family, &
                       lzone%grid%umesh%boco(ib)%family)) nbc = ib
        enddo
        ! Correction de flux
        if (cumulreste == oui) then
          call corr_varprim(lzone%grid%field, lzone%grid%umesh, &
                          lzone%defsolver, &
                          lzone%coupling(ic)%zcoupling%etatcons, nbc, &
                          part_cor, typ_cor, .false.)
        else
          call corr_varprim(lzone%grid%field, lzone%grid%umesh, &
                          lzone%defsolver, &
                          lzone%coupling(ic)%zcoupling%etatcons, nbc, &
                          part_cor, typ_cor, lzone%info%end_cycle)
        endif 
      endif
    enddo
  endif

  ! On peut ici coder differentes methodes d'integration (RK, temps dual...)

  ! ---
  select case(lzone%defsolver%typ_solver)
  case(solKDIF, solNS)
    call integration_zone(dt, lzone)
  case(solVORTEX)
    call integration_zone_lag(dt, lzone)
  case default
    call erreur("Internal error","unknown solver")
  endselect
  ! ---

  !do if = 1, lzone%ndom  ! DEV: a remplacer par une boucle sur les grilles
                          ! apres homogeneisation des solveurs dans MGRID
    select case(lzone%defsolver%typ_solver)
    case(solKDIF, solNS)
      call update_champ(lzone%info, lzone%grid%info%field_loc, &
                        lzone%grid%umesh%ncell_int)  ! maj  des var. conservatives
    case(solVORTEX)
      ! pas de mise a jour pour le moment 
      ! DEV : mise a jour de la position des vortex libres
      ! call update_lag
      lzone%info%residumax  = 2._krp      ! astuce pour n'imposer qu'une iteration 
      lzone%info%cur_res    = 1.e-8_krp   ! dans le cycle
      lzone%info%residu_ref = 1.e+8_krp   ! astuce pour n'avoir qu'un cycle
    case default
      call erreur("Internal error","unknown solver")
    endselect
  !enddo

  ! ecriture d'informations et test de lzone%info%end_cycle de cycle

  call check_end_cycle(lzone%info, dt)

  call capteurs(lzone)

enddo

write(str_w,'(a,i8,a)') "    end of cycle integration within ",lzone%info%iter_loc," iterations"
call print_info(9,str_w)

!---------------------------------------
endsubroutine integrationmacro_zone

!------------------------------------------------------------------------------!
! Changes history
!
! juil 2002 : creation de la procedure
! juin 2003 : champs multiples
! juil 2003 : calcul du nombre de Fourier de la zone 
!             allocation des residus remontee a integration_macrodt
! sept 2003 : calcul des gradients
! oct  2003 : deplacement des proc. calc_gradient et calc_varprim dans integration_zone
! mars 2004 : integration de zone par technique lagrangienne
! oct  2004 : field chained list
! may  2006 : restructuration (to end_cycle subroutine)
! Fev  2007 : English translation
!------------------------------------------------------------------------------!

