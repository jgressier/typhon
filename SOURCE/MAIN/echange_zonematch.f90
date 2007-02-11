!------------------------------------------------------------------------------!
! Procedure : echange_zonematch           Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Janvier 2004
!   Echange des donnees entre deux zones
!
! Defauts/Limitations/Divers : pour l'instant, une methode de calcul commune 
!				aux deux zones
!------------------------------------------------------------------------------!

! -----------------PROVISOIRE-----------------------------------------------
!subroutine echange_zonematch(zone1, zone2, typcalc, nfacelim, nbc1, nbc2, ncoupl1, ncoupl2, icycle, typtemps, dtexch)
! --------------------------------------------------------------------------
subroutine echange_zonematch(zone1, zone2, typcalc, nfacelim, nbc1, nbc2, ncoupl1, ncoupl2, typtemps, dtexch)
 
use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrees --
type(st_zone)              :: zone1, zone2
integer                    :: typcalc             ! type d'interpolation
integer                    :: nfacelim            ! nombre de faces limites
integer                    :: nbc1, nbc2          ! indice des conditions aux limites 
                                                  ! concernees dans les zones 1 et 2                                                  
integer                    :: ncoupl1, ncoupl2    ! numero (identite) du raccord
                                                  ! dans les zones 1 et 2      
character                  :: typtemps
real(krp)                  :: dtexch             ! pas de temps entre 
                                                 ! deux echanges

! -----------------PROVISOIRE-----------------------------------------------
!integer     :: icycle
!---------------------------------------------------------------------------



! -- Declaration des sorties --

! -- Declaration des variables internes --
integer                        :: i, if, ic, if2, ifield, ic1, ic2, ip
type(v3d), dimension(nfacelim) :: normale ! normales a l'interface
type(v3d), dimension(nfacelim) :: vecinter ! vecteurs inter-cellule
    real(krp), dimension(nfacelim) :: d1, d2  ! distance centre de cellule - centre de face
  				                 ! (gauche, droite)  
integer                        :: typmethod
type(v3d)                      :: cg1, cg2, cgface ! centres des cellules des zones 1 et 2, et des faces
integer                        :: typsolver1, typsolver2
real(krp)                      :: dif_enflux     ! difference des energies d'interface dans les deuxzones
real(krp), dimension(nfacelim) :: corcoef   ! coefficient de correction de flux
real(krp)                      :: rap_f ! rapport des nb de Fourier des 2 zones
real(krp)                      :: fcycle1, fcycle2 ! nb de Fourier de cycle
                                                   ! des deux zones
integer                        :: placement ! variable pour le
                                            ! placement des corrections
real(krp)                      :: part_cor1, part_cor2 ! part de correction a
                                                  ! apporter, dans les 2 zones
integer                        :: typ_cor1, typ_cor2 ! type de correction

! -----------------PROVISOIRE-----------------------------------------------
!integer     :: uf
!---------------------------------------------------------------------------

! -- Debut de la procedure --

! -----------------PROVISOIRE------------------------------------------------
!  uf = 556
!if ((icycle.lt.10)) then
!  open(unit = uf, file = "t"//trim(adjustl(strof(icycle,3)))//".dat", form = 'formatted')
!  write(uf, '(a)') 'VARIABLES="X","Y","Z", "T"'
!
!  call output_field(uf, zone1%ust_mesh, zone2%ust_mesh, zone1%field, &
!                    zone2%field,"FIN DU CYCLE PRECEDENT")
!endif
!-----------------------------------------------------------------------------

typ_cor1 = zone1%coupling(ncoupl1)%typ_cor
typ_cor2 = zone2%coupling(ncoupl2)%typ_cor

part_cor1 = zone1%coupling(ncoupl1)%partcor
part_cor2 = zone2%coupling(ncoupl2)%partcor

if (typ_cor1 .ne. typ_cor2) then 
  call erreur("DEVELOPPEMENT", &
              "Types de correction differents non implementes")
endif

select case(typtemps)

  case(instationnaire) ! On applique des corrections de flux entre les echanges
    call choixcorrection(zone1, zone2, placement, corcoef, typ_cor1, nfacelim,&
                         nbc1, nbc2, ncoupl2)

    ! Correction
    if (placement == avant) then
! DEBUG
print*, "CORRECTION AVANT"
      call correction(zone1, zone2, nfacelim, corcoef, nbc1, nbc2, ncoupl1, &
                    ncoupl2, part_cor1, part_cor2, typ_cor1, typ_cor2, &
                    .false., dtexch)
    endif

endselect

! -----------------PROVISOIRE------------------------------------------------
!if ((icycle.lt.10)) then
!  call output_field(uf, zone1%ust_mesh, zone2%ust_mesh, zone1%field, &
!                    zone2%field,"APRES CORRECTION")
!endif
!-----------------------------------------------------------------------------


typsolver1 = zone1%defsolver%typ_solver
typsolver2 = zone2%defsolver%typ_solver

! Donnees geometriques :

do i=1, nfacelim    
  
  ! indices des faces concernees
  if  = zone1%gridlist%first%umesh%boco(nbc1)%iface(i)
  if2 = zone2%gridlist%first%umesh%boco(nbc2)%iface(zone2%coupling(ncoupl2)%zcoupling%connface(i))
  
  normale(i) = zone1%gridlist%first%umesh%mesh%iface(if,1,1)%normale
 
  cgface = zone1%gridlist%first%umesh%mesh%iface(if,1,1)%centre
  ic = zone1%gridlist%first%umesh%facecell%fils(if,1)
  cg1 = zone1%gridlist%first%umesh%mesh%centre(ic,1,1)
  ic = zone2%gridlist%first%umesh%facecell%fils(if2,1)
  cg2 = zone2%gridlist%first%umesh%mesh%centre(ic,1,1)

  ! calcul du vecteur unitaire "inter-cellules"
  vecinter(i) = (cg2 - cg1) / abs((cg2 - cg1))
  
  ! calcul des distances d1 et d2 entre les cellules (des zones 1 et 2) et l'interface.
  !d1(i) = (cgface-cg1).scal.vecinter(i)
  !d2(i) = (cg2-cgface).scal.vecinter(i)
  d1(i) = (cgface-cg1).scal.(cgface-cg1)/abs((cgface-cg1).scal.normale(i))
  d2(i) = (cgface-cg2).scal.(cgface-cg2)/abs((cgface-cg2).scal.normale(i))

enddo 

! Type de methode de calcul:
typmethod = zone1%defsolver%boco(zone1%gridlist%first%umesh%boco(nbc1)%idefboco)%typ_calc
! = zone2%defsolver%boco(zone2%gridlist%first%umesh%boco(nbc2)%idefboco)%typ_calc

! Valeurs des donnees instationnaires a echanger
call donnees_echange(zone1%coupling(ncoupl1)%zcoupling%solvercoupling, &
                     zone1%coupling(ncoupl1)%zcoupling%echdata, &
                     zone1, nbc1, zone2%coupling(ncoupl2)%zcoupling%echdata, &
                     zone2, nbc2, ncoupl1, ncoupl2, typ_cor1)


! Calcul des conditions de raccord
call echange(zone1%coupling(ncoupl1)%zcoupling%echdata, &
             zone2%coupling(ncoupl2)%zcoupling%echdata, &
             normale, vecinter, d1, d2, nfacelim, typcalc, typmethod,&
             typsolver1, typsolver2, &
             zone1%defsolver%boco(zone1%gridlist%first%umesh%boco(nbc1)%idefboco), &
             zone2%defsolver%boco(zone2%gridlist%first%umesh%boco(nbc2)%idefboco), &
             zone2%coupling(ncoupl2)%zcoupling%connface)

select case(typtemps)

  case(instationnaire) ! On applique des corrections de flux entre les echanges

    if (placement == apres) then
! DEBUG
print*, "CORRECTION APRES"

      if (typ_cor1 .ne. bocoT) then
        call correction(zone1, zone2, nfacelim, corcoef, nbc1, nbc2, ncoupl1, &
                        ncoupl2, part_cor1, part_cor2, typ_cor1, typ_cor2, &
                        .false., dtexch)
      endif
    endif

endselect



endsubroutine echange_zonematch

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2003 : creation de la procedure
! juil 2003 : ajouts pour corrections de  flux
! oct  2003 : ajout coef correction de flux
! oct  2003 : correction de flux seulement pour le cas instationnaire
! jan  2004 : orientation vers des corrections de flux avant ou apres
!             le calcul des quantites d'interface selon les cas
! fev  2004 : procedures choixcorrection, correction
!------------------------------------------------------------------------------!
