!------------------------------------------------------------------------------!
! Procedure : echange_zonematch           Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juillet 2003
!   Echange des donnees entre deux zones
!
! Defauts/Limitations/Divers : pour l'instant, une methode de calcul commune 
!				aux deux zones
!------------------------------------------------------------------------------!

subroutine echange_zonematch(zone1, zone2, typcalc, nfacelim, nbc1, nbc2, ncoupl1, ncoupl2, corcoef)

use OUTPUT
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrées --
type(st_zone)              :: zone1, zone2
integer                    :: typcalc             ! type d'interpolation
integer                    :: nfacelim            ! nombre de faces limites
integer                    :: nbc1, nbc2          ! indice des conditions aux limites 
                                                  ! concernées dans les zones 1 et 2                                                  
integer                    :: ncoupl1, ncoupl2    ! numéro (identité) du raccord
                                                  ! dans les zones 1 et 2                                                 
real(krp)                  :: corcoef             ! coefficient de correction de flux

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer                        :: i, if, ic, if2, ifield
type(v3d), dimension(nfacelim) :: normale ! normales à l'interface
type(v3d), dimension(nfacelim) :: vecinter ! vecteurs inter-cellule
    real(krp), dimension(nfacelim) :: d1, d2  ! distance centre de cellule - centre de face
  				                 ! (gauche, droite)  
integer                        :: typmethod
type(v3d)                      :: cg1, cg2, cgface ! centres des cellules des zones 1 et 2, et des faces
integer                        :: typsolver1, typsolver2
real(krp)                      :: dif_enflux     ! différence des énergies d'interface dans les deuxzones

! -- Debut de la procedure --

! Supplément de flux pour les échanges espacés : calcul de la différence à appliquer

call calcdifflux(zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal, &
                 zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal, &
                 nfacelim, zone1%coupling(ncoupl1)%zcoupling%solvercoupling, &
                 corcoef )

! Calcul des variables primitives avec correction de flux
do ifield = 1, zone1%ndom
  call corr_varprim(zone1%field(ifield), &
                    zone1%ust_mesh, &
                    zone1%defsolver, &
                    zone1%coupling(ncoupl1)%zcoupling%etatcons, nbc1)
enddo

do ifield = 1, zone2%ndom
  call corr_varprim(zone2%field(ifield), &
                    zone2%ust_mesh, &
                    zone2%defsolver, &
                    zone2%coupling(ncoupl2)%zcoupling%etatcons, nbc2)
enddo


typsolver1 = zone1%defsolver%typ_solver
typsolver2 = zone2%defsolver%typ_solver

! Données géométriques :

do i=1, nfacelim    
  
  ! indices des faces concernées
  if = zone1%ust_mesh%boco(nbc1)%iface(i)
  if2 = zone2%ust_mesh%boco(nbc2)%iface(i)
  
  normale(i) = zone1%ust_mesh%mesh%iface(if,1,1)%normale
 
  cgface = zone1%ust_mesh%mesh%iface(if,1,1)%centre
  ic = zone1%ust_mesh%facecell%fils(if,1)
  cg1 = zone1%ust_mesh%mesh%centre(ic,1,1)
  ic = zone2%ust_mesh%facecell%fils(if2,1)
  cg2 = zone2%ust_mesh%mesh%centre(ic,1,1)

  ! calcul du vecteur unitaire "inter-cellules"
  vecinter(i) = (cg2 - cg1) / abs((cg2 - cg1))
  
  ! calcul des distances d1 et d2 entre les cellules (des zones 1 et 2) et l'interface.
  d1(i) = (cgface-cg1).scal.vecinter(i)
  d2(i) = (cg2-cgface).scal.vecinter(i)

enddo 

! Type de méthode de calcul:
typmethod = zone1%defsolver%boco(zone1%ust_mesh%boco(nbc1)%idefboco)%typ_calc
! = zone2%defsolver%boco(zone2%ust_mesh%boco(nbc2)%idefboco)%typ_calc

! Valeurs des données instationnaires à échanger
call donnees_echange(zone1%coupling(ncoupl1)%zcoupling%solvercoupling, &
                     zone1%coupling(ncoupl1)%zcoupling%echdata, &
                     zone1, nbc1)
call donnees_echange(zone2%coupling(ncoupl2)%zcoupling%solvercoupling, &
                     zone2%coupling(ncoupl2)%zcoupling%echdata, &
                     zone2, nbc2)


! Calcul des conditions de raccord
!if (senseur(i)%sens) then
call echange(zone1%coupling(ncoupl1)%zcoupling%echdata, &
             zone2%coupling(ncoupl2)%zcoupling%echdata, &
             normale, vecinter, d1, d2, nfacelim, typcalc, typmethod,&
             zone1%coupling(ncoupl1)%zcoupling%cond_coupling, &
             zone2%coupling(ncoupl2)%zcoupling%cond_coupling, &
             zone1%coupling(ncoupl1)%zcoupling%solvercoupling)

!endif

endsubroutine echange_zonematch

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 (v0.0.1b): création de la procédure
! juillet 2003      : ajouts pour corrections de  flux
! oct 2003          : ajout coef correction de flux
!------------------------------------------------------------------------------!
