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

  endselect

  ! On peut ici coder différentes méthodes d'intégration (RK, temps dual...)

  ! ---
  call integration_zone(dt, lzone)
  ! ---

  do if = 1, lzone%ndom
    call update_champ(lzone%info, lzone%field(if), lzone%ust_mesh%ncell_int)  ! màj  des var. conservatives
   ! !call calc_varprim(lzone%defsolver, lzone%field(if))     ! calcul des var. primitives
   ! 
   ! ! on ne calcule les gradients que dans les cas nécessaires
   ! if (lzone%defspat%calc_grad) then
   !   call calc_gradient(lzone%defsolver, lzone%ust_mesh,                 &
   !                      lzone%field(if)%etatprim, lzone%field(if)%gradient)
   ! endif
  enddo

  ! écriture d'informations et test de fin de cycle

  select case(lzone%info%typ_temps)

  case(stationnaire)
    lzone%info%residu_ref = max(lzone%info%residu_ref, lzone%info%cur_res)
    if (lzone%info%cur_res/lzone%info%residu_ref <= lzone%info%residumax) fin = .true.
    write(str_w,'(a,i5,a,g10.4)') "    iteration",iter," | residu = ", &
                                  log10(lzone%info%cur_res/lzone%info%residu_ref)

  case(instationnaire)
    local_t = local_t + dt
    write(str_w,'(a,i5,a,g10.4)') "    integration",iter," à t local =",local_t
  
  case(periodique)

  endselect

  call print_info(9,str_w)

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
!------------------------------------------------------------------------------!
