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
subroutine integration_cycle(lworld, exchcycle, ncoupling, itc)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
integer                         :: ncoupling  ! nombre de couplages
integer, dimension(1:ncoupling) :: exchcycle  ! indice du cycle d'echange
                                              ! (pour les differents couplages de zones)
integer                         :: itc        ! iteration du cycle

! -- Declaration des entrees/sorties --
type(st_world) :: lworld

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: izone, ir, ifield, if
integer   :: iz1, iz2, ic, ncoupl1, ncoupl2, ib, nbc1, nbc2

! -- Debut de la procedure --


! allocation des champs de residus
!do izone = 1, lworld%prj%nzone
!  do if = 1, lworld%zone(izone)%ndom
!    call alloc_res(lworld%zone(izone)%info%field_loc(if))
!  enddo
!enddo

! -- Procedure d'echange, en debut de cycle

if (.not. lworld%info%cvloc) then

  if (ncoupling > 0) then

  do ir = 1, ncoupling

    if (lworld%info%icycle.eq.exchcycle(ir)) then

      ! calcul des donnees de raccord : indices de raccord, de CL pour 
      ! les deux zones couplees
      call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

      ! appel procedure d'echange
      call echange_zonedata(lworld,ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

      select case(lworld%prj%typ_temps)
     
       case(instationnaire)
       ! reinitialisation a 0 des tableaux de cumul de flux pour la correction 
       ! de flux
       print*, "correction de flux", lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)%scal(1), &
               lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(1), &
               lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(1)
       lworld%zone(iz1)%coupling(ncoupl1)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp
       lworld%zone(iz2)%coupling(ncoupl2)%zcoupling%etatcons%tabscal(1)%scal(:) = 0._krp

      endselect

    endif

  enddo

  call output_result(lworld, in_cycle) !DEV2602

  endif

endif

!------------------------------------------------------------------------------
! DVT : comparaison des flux a l'interface.
!------------------------------------------------------------------------------
!
! Calcul des conditions aux limites pour le calcul des flux a l'interface
!
!do izone = 1, lworld%prj%nzone
! call conditions_limites(lworld%zone(izone))
!enddo
!
!if (lworld%prj%ncoupling > 0) then
!
!ir =1 ! DVT : provisoire
!    
! calcul des donnees de raccord : indices de raccord, de CL pour les 
! deux zones couplees

!call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)
!
!call comp_flux(lworld%zone(iz1), lworld%zone(iz2), nbc1, nbc2, lworld%zone(iz1)%ust_mesh%boco(nbc1)%nface,  &
!                    lworld%info%curtps, ncoupl1)
!endif
!

!------------------------------------------------------------------------------


! -- Critere de realisation du cycle - 27/10/04
call crit_calc_cycle(lworld, ncoupling, lworld%info%cvloc, itc)

! -- Convergence locale : reinitialisation des champs a ceux de debut de cycle
!    - 18/10/04
if (.not. lworld%info%cvloc) then
  if (lworld%prj%it_cycle == iterate) then
    do izone = 1, lworld%prj%nzone
      call transfer_field(lworld%zone(izone)%grid%info%field_loc, &
                                    lworld%zone(izone)%grid%info%field_cyclestart)
    enddo
  endif
endif


! -- Calcul du nouvel instant d'échange - 18/10/04
if ((lworld%info%cvloc).or.(lworld%prj%it_cycle == noiterate)) then
  do ir = 1, ncoupling
    if (lworld%info%icycle.eq.exchcycle(ir)) then
      exchcycle(ir) = exchcycle(ir) + lworld%coupling(ir)%n_tpsbase
      print*, "DEBUG n° cycle échange", exchcycle(ir)
    endif
  enddo
endif

if (.not. lworld%info%cvloc) then

  write(str_w,'(a,i5)') " ITÉRATION DE CYCLE N°", itc
  call print_info(6,str_w)

! --------------------------------------------
! Integration d'un cycle de chacune des zones 
! --------------------------------------------

! -- Initialisation du residu courant de world a 0 :
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

  ! -- Initialisation de residu_reforigine : valeur du residu de reference 
  !    du premier cycle pour chaque zone.
    if(lworld%info%icycle.eq.1) then
      lworld%zone(izone)%info%residu_reforigine = lworld%zone(izone)%info%residu_ref
    endif

  ! -- Retour d'informations d'integration du cycle

    lworld%zone(izone)%info%typ_temps = lworld%prj%typ_temps

    select case(lworld%prj%typ_temps)

    case(stationnaire)

   
  ! On attribue les residus courant et de reference de la zone la moins avancee, c'est-a-dire
  ! celle dont le rapport (residu courant/residu de reference (d'origine)) est le plus grand.
  ! Ainsi, la fin est atteinte quand toutes les zones ont vu leur residu diminuer de la valeur
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
  !   call dealloc_res(lworld%zone(izone)%info%field_loc(if))
  ! enddo
  enddo

endif

!-------------------------------------
endsubroutine integration_cycle

!------------------------------------------------------------------------------!
! Historique des modifications
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
! oct  2004 : local convergence
!------------------------------------------------------------------------------!
