!------------------------------------------------------------------------------!
! Procedure : integrationmacro_zone       Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf Historique)
!   Intégration d'une zone sur un écart de temps donné,
!   d'une représentation physique uniquement
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integrationmacro_zone(lzone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone            ! zone à intégrer

! -- Declaration des sorties --

! -- Declaration des variables internes --
real(krp)   :: local_t            ! temps local (0 à mdt)
real(krp)   :: dt                 ! pas de temps de la zone
integer     :: iter               ! numéro d'itération local au cycle
integer     :: if                 ! index de champ
real(krp)   :: fourier
logical     :: fin

! -- Debut de la procedure --

iter    = 0
local_t = 0._krp
fin     = .false.

! écriture d'informations

select case(lzone%info%typ_temps)

case(stationnaire)
  write(str_w,'(a,i5)') "  zone",lzone%id

case(instationnaire)
  write(str_w,'(a,i5,a,g10.4)') "  zone",lzone%id," à t local =",local_t
  
case(periodique)

endselect

call print_info(7,str_w)

!----------------------------------
! intégration
!----------------------------------

do while (.not.fin)

  iter = iter + 1
  
  ! ---
  call calc_zonetimestep(lzone, dt)
  ! ---

  ! écriture d'informations et gestion

  select case(lzone%info%typ_temps)

  case(stationnaire)

  case(instationnaire)
    if (dt >= (lzone%info%cycle_dt - local_t)) then
      fin = .true.
      dt  = lzone%info%cycle_dt - local_t
    endif  
  
  case(periodique)
    call erreur("Développement","cas non implémenté")

  endselect

  ! On peut ici coder différentes méthodes d'intégration (RK, temps dual...)

  ! ---
  select case(lzone%defsolver%typ_solver)
  case(solKDIF)
    call integration_zone(dt, lzone)
  case(solVORTEX)
    call integration_zone_lag(dt, lzone)
  endselect
  ! ---

  !do if = 1, lzone%ndom  ! DEV: à remplacer par une boucle sur les grilles
                          ! après homogénéisation des solveurs dans MGRID
    select case(lzone%defsolver%typ_solver)
    case(solKDIF)
      call update_champ(lzone%info, lzone%grid%field, &
                        lzone%grid%umesh%ncell_int)  ! màj  des var. conservatives
    case(solVORTEX)
      ! pas de mise à jour pour le moment 
      ! DEV : mise à jour de la position des vortex libres
      ! call update_lag
      lzone%info%residumax  = 2._krp      ! astuce pour n'imposer qu'une itération 
      lzone%info%cur_res    = 1.e-8_krp   ! dans le cycle
      lzone%info%residu_ref = 1.e+8_krp   ! astuce pour n'avoir qu'un cycle
    endselect
  !enddo

  ! écriture d'informations et test de fin de cycle

  select case(lzone%info%typ_temps)

  case(stationnaire)
    lzone%info%residu_ref = max(lzone%info%residu_ref, lzone%info%cur_res)
    if (lzone%info%cur_res/lzone%info%residu_ref <= lzone%info%residumax) fin = .true.
    if (mod(iter,10) == 0) &
      write(str_w,'(a,i5,a,g10.4)') "    iteration",iter," | residu = ", &
                                    log10(lzone%info%cur_res/lzone%info%residu_ref)

  case(instationnaire)
    local_t = local_t + dt
    if (mod(iter,10) == 0) &
      write(str_w,'(a,i5,a,g10.4)') "    integration",iter," à t local =",local_t
  
  case(periodique)

  endselect

  if (mod(iter,10) == 0) call print_info(9,str_w)

enddo

call capteurs(lzone)

!---------------------------------------
endsubroutine integrationmacro_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2002 : création de la procédure
! juin 2003 : champs multiples
! juil 2003 : calcul du nombre de Fourier de la zone 
!             allocation des residus remontée à integration_macrodt
! sept 2003 : calcul des gradients
! oct  2003 : déplacement des proc. calc_gradient et calc_varprim dans integration_zone
! mars 2004 : integration de zone par technique lagrangienne
!------------------------------------------------------------------------------!
