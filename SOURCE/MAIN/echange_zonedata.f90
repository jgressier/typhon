!------------------------------------------------------------------------------!
! Procedure : echange_zonedata            Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Echange de données entre zones de calcul
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

! -- Declaration des entrées         --
integer             :: ir               ! indice du couplage
integer             :: iz1, iz2         ! indices des zones
integer             :: ncoupl1, ncoupl2 ! indices des couplages des zones
integer             :: nbc1, nbc2       ! indices des conditions limites des zones

! -- Declaration des entrées/sorties --
type(st_world)      :: lworld

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(lworld%coupling(ir)%typ_calc)
   
case(mesh_match)
!print*,"!!! DEBUG echange zone data : maillages coincidants "  
call echange_zonematch(lworld%zone(iz1), lworld%zone(iz2), &
                      lworld%coupling(ir)%typ_interpol, &
                      lworld%zone(iz1)%ust_mesh%boco(nbc1)%nface,&
                      nbc1, nbc2, ncoupl1, ncoupl2, &
                      lworld%coupling(ir)%corcoef )
  
case(mesh_nonmatch)
call erreur("Développement","'nonmatch' : Cas non implémenté")
    
case(mesh_slide)
call erreur("Développement","'slide' : Cas non implémenté")
    
case default
call erreur("Développement (echange_zonedata)","Cas non implémenté")
  
endselect 

endsubroutine echange_zonedata

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 (v0.0.1b): création de la procédure
! oct 2003          : ajout coef correction de flux
!------------------------------------------------------------------------------!
