!------------------------------------------------------------------------------!
! Procedure : integration_cycle           Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf Historique)
!   Integration de cycle pour toutes les zones
!   Gestion des communications et couplages entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integration_cycle(lworld, exchcycle, ncoupling)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
integer                         :: ncoupling  ! nombre de couplages
integer, dimension(1:ncoupling) :: exchcycle  ! indice du cycle d'echange
                                              ! (pour les differents couplages de zones)

! -- Declaration des entrees/sorties --
type(st_world) :: lworld

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: izone, ir
integer   :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2
real(krp) :: wcur_res

! -- Body --

! -- Procedure d'echange, en debut de cycle

if (ncoupling > 0) then

do ir = 1, ncoupling

  if (lworld%info%icycle.eq.exchcycle(ir)) then

    ! calcul des donnees de raccord : indices de raccord, de CL pour 
    ! les deux zones couplees
    call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

    ! appel procedure d'echange
    call echange_zonedata(lworld,ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

    select case(lworld%prj%time_model)
     
    case(time_unsteady)
      lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp
      lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp

    endselect

    ! calcul du nouvel instant d'echange
    exchcycle(ir) = exchcycle(ir) + lworld%coupling(ir)%n_tpsbase

  endif

enddo

call output_result(lworld, in_cycle) !DEV2602 !!

endif


!------------------------------------------------------------------------------

! --------------------------------------------
! CYCLE integration : loop on zones
! --------------------------------------------

! -- Initialisation du residu courant de world a 0 :
wcur_res = lworld%info%cur_res
lworld%info%cur_res = tiny(lworld%info%cur_res)

do izone = 1, lworld%prj%nzone
 
  ! -- Initialisation des infos pour le cycle

  select case(lworld%prj%time_model)

  case(time_steady)
    lworld%zone(izone)%info%residumax  = lworld%zone(izone)%defsolver%deftime%maxres
    lworld%zone(izone)%info%maxit      = lworld%zone(izone)%defsolver%deftime%maxit
    lworld%zone(izone)%info%residu_ref = lworld%info%cur_res

  case(time_unsteady)
   lworld%zone(izone)%info%cycle_start = lworld%info%curtps
   lworld%zone(izone)%info%cycle_dt    = lworld%prj%dtbase

  case default
    call erreur("Development", "unknown TIME model")
  endselect

  !-------------------------------------
  call integration_cyclezone(lworld%zone(izone), lworld%info%residu_ref, wcur_res)
  !-------------------------------------

  ! -- Initialisation de residu_reforigine : valeur du residu de reference 
  !    du premier cycle pour chaque zone.
  if(lworld%info%icycle.eq.1) then
    lworld%zone(izone)%info%residu_reforigine = lworld%zone(izone)%info%residu_ref
  endif

  ! -- Retour d'informations d'integration du cycle

  lworld%zone(izone)%info%time_model = lworld%prj%time_model

  select case(lworld%prj%time_model)

  case(time_steady)
   
  ! On attribue les residus courant et de reference de la zone la moins avancee, c'est-a-dire
  ! celle dont le rapport (residu courant/residu de reference (d'origine)) est le plus grand.
  ! Ainsi, la fin est atteinte quand toutes les zones ont vu leur residu diminuer de la valeur
  ! voulue au moins.

    if( (lworld%zone(izone)%info%cur_res / lworld%zone(izone)%info%residu_reforigine) &
        .ge.(lworld%info%cur_res / lworld%info%residu_ref) ) then
      lworld%info%cur_res    = lworld%zone(izone)%info%cur_res
      lworld%info%residu_ref = lworld%zone(izone)%info%residu_reforigine !max(lworld%info%residu_ref, lworld%zone(izone)%info%residu_ref)
    endif

  case(time_unsteady)
    lworld%zone(izone)%info%cycle_dt = lworld%prj%dtbase

  case default
    call erreur("internal error (integration_cycle)","unknown time model")
  endselect
  ! do if = 1, lworld%zone(izone)%ndom
  !   call dealloc_res(lworld%zone(izone)%field(if))
  ! enddo
enddo


!-------------------------------------
endsubroutine integration_cycle

!------------------------------------------------------------------------------!
! Changes history
!
! juil 2002 : creation de la procedure
! mai  2003 : procedures d'echange
! juil 2003 : ajout de corrections de flux lors de couplage
!             allocation des residus globale pour tous les cycles
! sept 2003 : changement de nom de la procedure (ancien: integration_macrodt)
!             gestion du calcul selon residus 
! oct  2003 : suppression correction de flux APRES
! oct  2003 : remplacement instant d'echange excht par cycle d'echange exchcycle
! oct  2003 : modification de la gestion selon residus pour le calcul stationnaire 
!             multizone.
! oct  2003 : corrections de flux seulement en instationnaire
!------------------------------------------------------------------------------!
