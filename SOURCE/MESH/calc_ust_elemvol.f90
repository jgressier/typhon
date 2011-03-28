!------------------------------------------------------------------------------!
! Procedure : calc_ust_elemvol            Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des volumes elementaires d'une cellule, definis comme des coniques
!   de base, chacune des faces et de sommet, le centre approximatif de cellule.
!   En sortie, on obtient le volume et le centre de gravite des volumes 
!   elementaires
!
! Defauts/Limitations/Divers :
!   En terme d'organisation memoire, on a choisi d'organiser les volumes
!   elementaires par face (2 volumes par face) plutot que de les rattacher
!   aux cellules (nombre de volumes indetermine par cellule)
!
!------------------------------------------------------------------------------!
subroutine calc_ust_elemvol(geom, ncell, nface, midcell, facecell, cgface, face, &
                            cg_elem, vol_elem)

use TYPHMAKE
use OUTPUT
use USTMESH
use MESHBASE

implicit none

! -- Declaration des entrees --
integer                     :: geom       ! type de geometrie
integer                     :: ncell      ! nombre de cellules
integer                     :: nface      ! nombre de faces
type(v3d), dimension(ncell) :: midcell    ! centres de cellule approches ( /= barycentre )
type(st_connect)         :: facecell   ! connectivite face->cellules
type(v3d), dimension(nface) :: cgface     ! barycentre de face
type(st_face), dimension(nface) &
                            :: face       ! faces (surfaces et normales)

! -- Declaration des sorties --
type(v3d), dimension(nface,2) :: cg_elem    ! centres de volume elementaire
real(krp), dimension(nface,2) :: vol_elem   ! volumes de volume elementaire

! -- Declaration des variables internes --
integer         :: if           ! indice  de face
integer         :: ic           ! indice  de cellule
real(krp)       :: kcg, kvol    ! coefficients pour le calcul du centre et du volume


! -- Debut de la procedure --

! le volume elementaire est defini par une base (la face) de centre H
! et un sommet K (centre approche (midcell) de la cellule adjacente). 
! On en deduit le volume et le centre de gravite du volume elementaire.

select case(geom)
case(2)
  kcg  = 1./3._krp
  kvol = 1./2._krp
case(3)
  kcg  = 1./4._krp
  kvol = 1./3._krp
case default
  call error_stop("internal error: unexpected geometry dimension in calc_ust_elemvol")
endselect

do if = 1, facecell%nbnodes

  ic = facecell%fils(if,1)
  call subcalc_elemvol(midcell(ic), cgface(if), face(if), &
                       cg_elem(if,1), vol_elem(if,1))

  ic = facecell%fils(if,2)
  if (ic /= 0) then
    call subcalc_elemvol(midcell(ic), cgface(if), face(if), &
                         cg_elem(if,2), vol_elem(if,2))
  else
    cg_elem (if,2) = v3d(0._krp, 0._krp, 0._krp)
    vol_elem(if,2) = 0._krp
  endif

  !! write(*,"(a,i3,8f8.1)")"elem_vol",if, cg_elem(if,1), vol_elem(if,1), cg_elem(if,2), vol_elem(if,2) !! DEBUG
enddo


contains

  !---------------------------------------------------------------------------------
  subroutine subcalc_elemvol(vK, vH, face, cg, vol)
    implicit none

    ! --- entrees ---
    type(v3d)       :: vK, vH      ! coord. K (sommet) et H (centre de face) 
    type(st_face)   :: face        ! face
    ! --- sorties ---
    type(v3d)       :: cg          ! centre de volume elementaire
    real(krp)       :: vol         ! volume du volume elementaire
    ! --- interne ---
    type(v3d)       :: vKH         ! vecteur KH (sommet-centre de face)
    real(krp)       :: hauteur     ! volume du volume elementaire

      vKH     = vH - vK
      hauteur = abs(vKH.scal.face%normale)

      cg      = vH - kcg*vKH
      vol     = kvol*hauteur*face%surface

  endsubroutine subcalc_elemvol

endsubroutine calc_ust_elemvol

!------------------------------------------------------------------------------!
! Historique des modifications
!
! jan  2002 : creation de la procedure
!------------------------------------------------------------------------------!

