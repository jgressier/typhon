!------------------------------------------------------------------------------!
! Procedure : echange_zonedata            Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Echange de donnees entre zones de calcul
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine echange_zonedata(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees         --
integer             :: ir               ! indice du couplage
integer             :: iz1, iz2         ! indices des zones
integer             :: ncoupl1, ncoupl2 ! indices des couplages des zones
integer             :: nbc1, nbc2       ! indices des conditions limites des zones

! -- Declaration des entrees/sorties --
type(st_world)      :: lworld

! -- Declaration des variables internes --
real(krp)           :: dtexch ! pas de temps entre deux echanges

! -- Debut de la procedure --

select case(lworld%coupling(ir)%typ_calc)
   
case(mesh_match)

! pas de temps entre deux echanges
dtexch = lworld%coupling(ir)%n_tpsbase*lworld%prj%dtbase 

! ----PROVISOIRE pour affichage des champs avt et apres cor de flux-------------
!call echange_zonematch(lworld%zone(iz1), lworld%zone(iz2), &
!                      lworld%coupling(ir)%typ_interpol, &
!                      lworld%zone(iz1)%ust_mesh%boco(nbc1)%nface,&
!                      nbc1, nbc2, ncoupl1, ncoupl2, lworld%info%icycle, &
!                      lworld%prj%typ_temps, lworld%prj%dtbase)
!-------------------------------------------------------------------------------

call echange_zonematch(lworld%zone(iz1), lworld%zone(iz2), &
                       lworld%coupling(ir)%typ_interpol, &
                       lworld%zone(iz1)%gridlist%first%umesh%boco(nbc1)%nface, &
                       nbc1, nbc2, ncoupl1, ncoupl2, lworld%prj%typ_temps, &
                       dtexch)
  
case(mesh_nonmatch)
  call erreur("Developpement","'nonmatch' : Cas non implemente")
    
case(mesh_slide)
  call erreur("Developpement","'slide' : Cas non implemente")
    
case default
  call erreur("Developpement (echange_zonedata)","Cas non implemente")
  
endselect 

endsubroutine echange_zonedata

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 : creation de la procedure
! oct 2003 : ajout coef correction de flux
! oct 2003 : ajout du type temporel d'integration pour choix dans
!            echange_zonematch
! avr 2004 : modification en structure MGRID, limite a une grille
!------------------------------------------------------------------------------!
