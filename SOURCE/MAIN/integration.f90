!------------------------------------------------------------------------------!
! Procedure : integration                 Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf historique)
!   Integration totale jusqu'au critère d'arrêt du calcul
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine integration(lworld)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées/sorties --
type(st_world) :: lworld

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des variables internes --
!real(krp)      :: macro_dt
real(krp), dimension(:), allocatable &
               :: excht ! instants d'échange pour les différents couplages de zones
integer        :: ir, izone, if
integer        :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- Debut de la procedure --

!macro_dt        = lworld%prj%dtbase

! initialisation

lworld%info%icycle          = 0
lworld%info%curtps          = 0._krp
lworld%info%residu_ref      = 0._krp
lworld%info%fin_integration = .false.


!if (lworld%prj%ncoupling > 0) then
  allocate(excht(lworld%prj%ncoupling))
  excht(:) = 0._krp
!endif

! allocation des champs de résidus
do izone = 1, lworld%prj%nzone
  do if = 1, lworld%zone(izone)%ndom
    call alloc_res(lworld%zone(izone)%field(if))
    !! DEV : l'allocation ne doit se faire que dans certains cas
    call alloc_grad(lworld%zone(izone)%field(if))
  enddo
enddo

! Faire appel à une subroutine contenant l'écriture
!------------------------------------------------------------------------------------------------
! DVT : Ouverture du fichier de comparaison des flux à l'interface
!------------------------------------------------------------------------------------------------
if (lworld%prj%ncoupling > 0) then
  open(unit = uf_compflux, file = "compflux.dat", form = 'formatted')
  write(uf_compflux, '(a)') 'VARIABLES="t","F1","F2", "ERREUR", "Fcalcule", "ERRCALC"'
endif


!--------------------------------------------------------
! INTEGRATION
!--------------------------------------------------------
do while (.not. lworld%info%fin_integration)

  lworld%info%icycle = lworld%info%icycle + 1

  ! -- écriture d'informations en début de cycle --

  select case(lworld%prj%typ_temps)
  case(stationnaire)
    write(str_w,'(a,i5)') "* Cycle", lworld%info%icycle
  case(instationnaire)
    write(str_w,'(a,i5,a,g10.4)') "* Cycle", lworld%info%icycle, &
                                  " : t = ",  lworld%info%curtps
  case(periodique)
    write(str_w,'(a,i5)') "* Cycle", lworld%info%icycle
  endselect

  call print_info(6,str_w)

  ! -- intégration d'un cycle --

  call integration_cycle(lworld, excht, lworld%prj%ncoupling)  
    
  ! -- écriture d'informations en fin de cycle --

  select case(lworld%prj%typ_temps)

  case(stationnaire)
    write(str_w,'(a,g10.4)') "  Résidu de cycle = ", log10(lworld%info%cur_res/lworld%info%residu_ref)
    if (lworld%info%cur_res/lworld%info%residu_ref <= lworld%prj%residumax) then
      lworld%info%fin_integration = .true.
    endif

  case(instationnaire)
    lworld%info%curtps = lworld%info%curtps + lworld%prj%dtbase
    if (lworld%info%curtps >= lworld%prj%duree) lworld%info%fin_integration = .true.
    write(str_w,'(a)') " "

  case(periodique)
    lworld%info%fin_integration = .true.

  endselect

  call print_info(6,str_w)

enddo

! Mise à jour des conditions aux limites, notamment de couplage pour l'affichage des données :
if (lworld%prj%ncoupling > 0) then
  do ir = 1, lworld%prj%ncoupling
      call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)
      call echange_zonedata(lworld,ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2) 
  enddo
endif

do izone = 1, lworld%prj%nzone
 call conditions_limites(lworld%zone(izone))
enddo

!-----------------------------------------------------------------------------------------------------------------------
! DVT : Fermeture du fichier de comparaison des flux à l'interface
!-----------------------------------------------------------------------------------------------------------------------
!if (lworld%prj%ncoupling > 0) then
  deallocate(excht)
  close(uf_compflux)
!endif
!-----------------------------------------------------------------------------------------------------------------------

do izone = 1, lworld%prj%nzone
 do if = 1, lworld%zone(izone)%ndom
   call dealloc_res(lworld%zone(izone)%field(if))
   call dealloc_grad(lworld%zone(izone)%field(if))
 enddo
enddo

endsubroutine integration

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2002 : création de la procédure
! juin 2003 : instant d'échange excht
!             mise à jour des CL pour le fichier de sortie
! sept 2003 : gestion du calcul par résidus (optionnel) + réorganisation
!------------------------------------------------------------------------------!
