!------------------------------------------------------------------------------!
! Procedure : integration                 Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : Mars 2003 (cf historique)
!   Integration des champs de chaque zone
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
real(krp)      :: macro_dt
real(krp), dimension(:), allocatable &
               :: excht ! instants d'échange pour les différents couplages de zones
integer        :: ir, izone

! -- Debut de la procedure --

macro_dt        = lworld%prj%dtbase

! initialisation

lworld%info%icycle          = 0
lworld%info%curtps          = 0._krp
lworld%info%fin_integration = .false.


!if (lworld%prj%ncoupling > 0) then
  allocate(excht(lworld%prj%ncoupling))
  excht(:) = 0._krp
!endif


!-----------------------------------------------------------------------------------------------------------------------
! DVT : Ouverture du fichier de comparaison des flux à l'interface
!-----------------------------------------------------------------------------------------------------------------------
if (lworld%prj%ncoupling > 0) then
  open(unit = uf_compflux, file = "compflux.dat", form = 'formatted')
  write(uf_compflux, '(a)') 'VARIABLES="t","F1","F2", "ERREUR", "Fcalcule", "ERRCALC"'
endif
!-----------------------------------------------------------------------------------------------------------------------
do while (.not. lworld%info%fin_integration)

  lworld%info%icycle = lworld%info%icycle + 1
  write(str_w,'(a,i5,a,g10.4)') "* Cycle", lworld%info%icycle, &
                                " : t = ",  lworld%info%curtps
  call print_info(6,str_w)

  call integration_macrodt(macro_dt, lworld, excht, lworld%prj%ncoupling)  
    
  lworld%info%curtps = lworld%info%curtps + macro_dt

  if (lworld%info%curtps >= lworld%prj%duree) then
    lworld%info%fin_integration = .true.
  endif

enddo

! Mise à jour des conditions aux limites, notamment de couplage pour l'affichage des données :
if (lworld%prj%ncoupling > 0) then
  do ir = 1, lworld%prj%ncoupling
      call echange_zonedata(lworld,ir)  
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
endsubroutine integration

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2002 (v0.0.1b): création de la procédure
! juin 2003          : instant d'échange excht
!                      mise à jour des CL pour le fichier de sortie
!------------------------------------------------------------------------------!
