!------------------------------------------------------------------------------!
! Procedure : echange_zonedata            Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Echange de données entre zones de calcul
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine echange_zonedata(lworld, ir)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées         --
integer             :: ir

! -- Declaration des entrées/sorties --
type(st_world)      :: lworld

! -- Declaration des variables internes --
integer                    :: ib, ic
integer                    :: iz1, iz2
integer                    :: ncoupl1, ncoupl2
integer                    :: nbc1, nbc2

! -- Debut de la procedure --

! Détermination des numéros des zones couplées
iz1 = lworld%coupling(ir)%zone1
iz2 = lworld%coupling(ir)%zone2
  
! Détermination des numéros du raccord pour les zones 1 et 2
do ic = 1, lworld%zone(iz1)%ncoupling
  if (samestring(lworld%zone(iz1)%coupling(ic)%connzone, lworld%zone(iz2)%nom)) then
    ncoupl1 = ic
  endif
enddo

do ic = 1, lworld%zone(iz2)%ncoupling
  if (samestring(lworld%zone(iz2)%coupling(ic)%connzone, lworld%zone(iz1)%nom)) then
    ncoupl2 = ic
  endif
enddo

! Détermination des indices de condition aux limites pour les zones 1 et 2
do ib = 1, lworld%zone(iz1)%ust_mesh%nboco
  if (samestring(lworld%zone(iz1)%coupling(ncoupl1)%family, lworld%zone(iz1)%ust_mesh%boco(ib)%family)) then
    nbc1 = ib
  endif
enddo
  
do ib = 1, lworld%zone(iz2)%ust_mesh%nboco
  if (samestring(lworld%zone(iz2)%coupling(ncoupl2)%family, lworld%zone(iz2)%ust_mesh%boco(ib)%family)) then
    nbc2 = ib
  endif
enddo

select case(lworld%coupling(ir)%typ_calc)
   
case(mesh_match)
!print*,"!!! DEBUG echange zone data : maillages coincidants "  
call echange_zonematch(lworld%zone(iz1), lworld%zone(iz2), &
                      lworld%coupling(ir)%typ_interpol, &
                      lworld%zone(iz1)%ust_mesh%boco(nbc1)%nface,&
                      nbc1, nbc2, ncoupl1, ncoupl2  )
  
case(mesh_nonmatch)
call erreur("Développement","'nonmatch' : Cas non implémenté")
    
case(mesh_slide)
call erreur("Développement","'slide' : Cas non implémenté")
    
case default
call erreur("Développement (echange_zonedata)","Cas non implémenté")
  
endselect 

endsubroutine echange_zonedata
