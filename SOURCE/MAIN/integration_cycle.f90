!------------------------------------------------------------------------------!
! Procedure : integration_cycle           Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf Historique)
!   Intégration de cycle pour toutes les zones
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

! -- Declaration des entrées --
integer                         :: ncoupling  ! nombre de couplages
integer, dimension(1:ncoupling) :: exchcycle  ! indice du cycle d'échange
                                              ! (pour les différents couplages de zones)

! -- Declaration des entrées/sorties --
type(st_world) :: lworld

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: izone, ir, ifield, if
integer   :: iz1, iz2, ic, ncoupl1, ncoupl2, ib, nbc1, nbc2

! -- Debut de la procedure --


! allocation des champs de résidus
!do izone = 1, lworld%prj%nzone
!  do if = 1, lworld%zone(izone)%ndom
!    call alloc_res(lworld%zone(izone)%field(if))
!  enddo
!enddo

! -- Procédure d'échange, en début de cycle

if (ncoupling > 0) then

do ir = 1, ncoupling

  if (lworld%info%icycle.eq.exchcycle(ir)) then

    ! calcul des données de raccord : indices de raccord, de CL pour 
    ! les deux zones couplées
    call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

    ! appel procédure d'échange
    call echange_zonedata(lworld,ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

    select case(lworld%prj%typ_temps)
     
     case(instationnaire)
     ! réinitialisation à 0 des tableaux de cumul de flux pour la correction 
     ! de flux
     print*, "correction de flux", lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)%scal(1), &
             lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(1), &
             lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(1)
     lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp
     lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp

    endselect

    ! calcul du nouvel instant d'échange
    exchcycle(ir) = exchcycle(ir) + lworld%coupling(ir)%n_tpsbase

  endif

enddo

call output_result(lworld, in_cycle) !DEV2602

endif


!------------------------------------------------------------------------------
! DVT : comparaison des flux à l'interface.
!------------------------------------------------------------------------------
!
! Calcul des conditions aux limites pour le calcul des flux à l'interface
!
!do izone = 1, lworld%prj%nzone
! call conditions_limites(lworld%zone(izone))
!enddo
!
!if (lworld%prj%ncoupling > 0) then
!
!ir =1 ! DVT : provisoire
!    
! calcul des données de raccord : indices de raccord, de CL pour les 
! deux zones couplées

!call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)
!
!call comp_flux(lworld%zone(iz1), lworld%zone(iz2), nbc1, nbc2, lworld%zone(iz1)%ust_mesh%boco(nbc1)%nface,  &
!                    lworld%info%curtps, ncoupl1)
!endif
!

!------------------------------------------------------------------------------

! --------------------------------------------
! Intégration d'un cycle de chacune des zones 
! --------------------------------------------

! -- Initialisation du résidu courant de world à 0 :
lworld%info%cur_res = 0

do izone = 1, lworld%prj%nzone
 
  ! -- Initialisation des infos pour le cycle

  lworld%zone(izone)%info%iter_tot  = 0
  lworld%zone(izone)%info%typ_temps = lworld%prj%typ_temps

  select case(lworld%prj%typ_temps)

  case(stationnaire)
    lworld%zone(izone)%info%residumax  = lworld%zone(izone)%deftime%maxres
    lworld%zone(izone)%info%residu_ref = 0._krp

  case(instationnaire)
    lworld%zone(izone)%info%cycle_dt = lworld%prj%dtbase

  case(periodique)

  endselect

  !-------------------------------------
  ! changement de nom en integration_cycle_zone ?
  call integrationmacro_zone(lworld%zone(izone))
  !-------------------------------------

  ! -- Initialisation de residu_reforigine : valeur du résidu de référence 
  !    du premier cycle pour chaque zone.
  if(lworld%info%icycle.eq.1) then
    lworld%zone(izone)%info%residu_reforigine = lworld%zone(izone)%info%residu_ref
  endif

  ! -- Retour d'informations d'intégration du cycle

  lworld%zone(izone)%info%typ_temps = lworld%prj%typ_temps

  select case(lworld%prj%typ_temps)

  case(stationnaire)

   
  ! On attribue les residus courant et de référence de la zone la moins avancée, c'est-à-dire
  ! celle dont le rapport (residu courant/residu de référence (d'origine)) est le plus grand.
  ! Ainsi, la fin est atteinte quand toutes les zones ont vu leur résidu diminuer de la valeur
  ! voulue au moins.

    if( (lworld%zone(izone)%info%cur_res / lworld%zone(izone)%info%residu_reforigine) &
        .ge.(lworld%info%cur_res / lworld%info%residu_ref) ) then
      lworld%info%cur_res    = lworld%zone(izone)%info%cur_res
      lworld%info%residu_ref = lworld%zone(izone)%info%residu_reforigine !max(lworld%info%residu_ref, lworld%zone(izone)%info%residu_ref)
    endif

  case(instationnaire)
    lworld%zone(izone)%info%cycle_dt = lworld%prj%dtbase

  case(periodique)

  endselect
  ! do if = 1, lworld%zone(izone)%ndom
  !   call dealloc_res(lworld%zone(izone)%field(if))
  ! enddo
enddo


!-------------------------------------
endsubroutine integration_cycle

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2002 : création de la procédure
! mai  2003 : procédures d'échange
! juil 2003 : ajout de corrections de flux lors de couplage
!             allocation des residus globale pour tous les cycles
! sept 2003 : changement de nom de la procédure (ancien: integration_macrodt)
!             gestion du calcul selon résidus 
! oct  2003 : suppression correction de flux APRES
! oct  2003 : remplacement instant d'échange excht par cycle d'échange exchcycle
! oct  2003 : modification de la gestion selon résidus pour le calcul stationnaire 
!             multizone.
! oct  2003 : corrections de flux seulement en instationnaire
!------------------------------------------------------------------------------!
