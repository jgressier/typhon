!------------------------------------------------------------------------------!
! MODULE : MESHGEOM                      Authors : J. Gressier
! 
! Fonction
!   Computation of geometrical mesh properties
!
!------------------------------------------------------------------------------!
module MESHGEOM

use MATH
use VEC3D
use TENSOR3
use MESHBASE
use USTMESH

implicit none

! -- Variables globales du module -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

! -- INTERFACES -------------------------------------------------------------

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : calc_ust_face               Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : see history
!   Calcul des faces
!   . normale (sens arbitraire) de la face  
!   . surface de la face
!------------------------------------------------------------------------------!
subroutine calc_ust_face(defmesh, facevtex, mesh)
implicit none
! -- INPUTS --
type(mnu_mesh)        :: defmesh      ! mesh parameters
type(st_connect)      :: facevtex
! -- INPUTS/OUTPUTS --
type(st_mesh)            :: mesh      ! entrees:vertex / sorties:iface
! -- OUTPUTS --

! -- Private DATA --
type(v3d), dimension(:), allocatable &
               :: vtex           ! sommets intermediaires
integer        :: nface          ! nombre de faces (connectivite et tableau)
integer        :: ns             ! nombre de sommet de la face (iface)
integer        :: if, is         ! index de face, index de sommet
real(krp)      :: surf, s1, s2   ! surfaces intermediaires
type(v3d)      :: norm           ! normale intermediaire
type(v3d)      :: pt, cg1, cg2   ! point et CG intermediaires

! -- BODY --

nface = facevtex%nbnodes          ! nombre de faces dans la connectivite
allocate(vtex(facevtex%nbfils))   ! nombre maximal de sommets par face

! A ce stage, on peut choisir de calculer les faces comme les volumes avec
! des decomposition en faces elementaires. Cela permet de calculer des
! surfaces de faces a nombre de sommets quelconque.
! Dans un premier temps, on se contente d'appliquer une methode specifique
! pour chaque type de face.

do if = 1, nface !> @optim make packet and vectorize if possible

  if (mesh%nfgauss > 1) call cfd_error("cannot handle more than 1 Gauss point") !> @dev define Gauss points on faces

  ns = count(facevtex%fils(if,1:facevtex%nbfils) /= 0)   ! number of actual nodes of this face
  do is = 1, ns
    vtex(is) = mesh%vertex(facevtex%fils(if,is),1,1)
  enddo

  ! calcul selon le nombre de sommets de la face

  select case(defmesh%geo)
  case(geo_2d, geo_2daxi)   ! --------------------- PLANAR 2D MESH and AXI

    select case(ns)
    case(2)
      norm = v3d(0._krp,0._krp,1._krp) .vect. (vtex(2)-vtex(1))
      surf = abs(norm)
      mesh%face_surf(if)     = surf
      mesh%face_normal(if,1) = norm / surf
      mesh%face_center(if,1) = .5_krp*(vtex(1) + vtex(2))
    case default
      call cfd_error("mesh property: only 2 vertices faces expected")
    endselect
      
!!$  case(geo_2daxi) ! -------------------- AXI 2D MESH
!!$
!!$    select case(ns)
!!$    case(2)
!!$      cgface(if) = .5_krp*(vtex(1) + vtex(2))
!!$      norm = v3d(0._krp,0._krp,1._krp) .vect. (vtex(2)-vtex(1))
!!$      surf = abs(norm)
!!$      mesh%iface(if,1,1)%normale = norm / surf
!!$      mesh%iface(if,1,1)%surface = surf * (two_pi*cgface(if)%y)    ! axisymmetric correction
!!$    case default
!!$      call cfd_error("mesh property: only 2 vertices faces expected")
!!$    endselect
      
  case(geo_3d)   ! -------------------- 3D MESH
 
    select case(ns)
    case(3)
      norm = .5_krp * ( (vtex(2)-vtex(1)) .vect. (vtex(3)-vtex(1)) )
      surf = abs(norm)
      mesh%face_surf(if)     = surf
      mesh%face_normal(if,1) = norm / surf
      mesh%face_center(if,1) = (vtex(1) + vtex(2) + vtex(3)) / 3._krp
    case(4)
      ! calcul du premier triangle elementaire
      norm = .5_krp * ( (vtex(2)-vtex(1)) .vect. (vtex(3)-vtex(1)) )
      s1   = abs(norm)
      cg1  = (vtex(1) + vtex(2) + vtex(3)) / 3._krp
      ! calcul du second triangle elementaire
      pt   = .5_krp * ( (vtex(3)-vtex(1)) .vect. (vtex(4)-vtex(1)) )
      s2   = abs(pt) 
      cg2  = (vtex(1) + vtex(3) + vtex(4)) / 3._krp
      ! calcul des normales et surfaces
      norm = norm + pt
      surf = abs(norm)
      mesh%face_surf(if)     = surf
      mesh%face_normal(if,1) = norm / surf
      mesh%face_center(if,1) = (s1*cg1 + s2*cg2)/(s1+s2)
    case default
      call cfd_error("mesh property: only 3 or 4 vertices faces expected")
    endselect

  case default
    call cfd_error("internal error: unexpected mesh type (MESHGEOM%calc_ust_face)")
  endselect

enddo

deallocate(vtex)

endsubroutine calc_ust_face

!------------------------------------------------------------------------------!
! Procedure : calc_ust_midcell            Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  :
!   calcul approximatif d'un centre de cellule (midcell) pour la definition
!   de volumes elementaires
!------------------------------------------------------------------------------!
subroutine calc_ust_midcell(umesh, midcell)
! -- INPUTS --
integer                 :: ncell      ! nombre de cellules
type(st_ustmesh)        :: umesh      ! ust mesh, facecell connectivity

! -- OUTPUTS --
type(v3d), dimension(*) :: midcell    ! centre de cellule ( /= barycentre )

! -- Private Data --
integer, dimension(:), allocatable &
                        :: nbface       ! nb de faces par cellule
integer                 :: if           ! indice  de face
integer                 :: ic, ic1, ic2 ! indices de cellules

! -- BODY --

! Pour chaque cellule, on veut faire la moyenne des barycentres (cgface) de chaque face 
! associee.
! Intuitivement, on aimerait, pour chaque cellule, ajouter la contribution de chacune
! des faces associees.
! Puisque c'est la connectivite face->cellules qui est donnee et non le contraire, on
! boucle sur les faces et ajoute les contributions a chaque cellule.
! (il faut faire attention aux faces limites qui ne sont connectees qu'a une cellule)
! Il faut compter le nombre de faces pour chaque cellules pour calculer le barycentre

! Initialisation

allocate(nbface(umesh%ncell_int))
nbface(:)                  = 0
midcell(1:umesh%ncell_int) = v3d(0._krp,0._krp,0._krp)

! boucle sur la liste des faces et sommation pour les cellules

do if = 1, umesh%nface

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)

  ! premiere cellule connectee
  midcell(ic1) = midcell(ic1) + umesh%mesh%face_center(if,1)
  nbface (ic1) = nbface (ic1) + 1

  ! seconde cellule connectee (si existante)
  if ((ic2 > 0).and.(ic2 <= umesh%ncell_int)) then
    midcell(ic2) = midcell(ic2) + umesh%mesh%face_center(if,1)
    nbface (ic2) = nbface (ic2) + 1
  endif

enddo

! calcul de la moyenne sur les cellules

do ic = 1, umesh%ncell_int
  midcell(ic) = midcell(ic) / real(nbface(ic),kind=krp)
enddo

deallocate(nbface)

endsubroutine calc_ust_midcell

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
!------------------------------------------------------------------------------!
subroutine calc_ust_elemvol(defmesh, umesh, midcell, cg_elem, vol_elem)
implicit none
! -- INPUTS --
type(mnu_mesh)      :: defmesh
type(st_ustmesh)    :: umesh      ! ust mesh, facecell connectivity
type(v3d)           :: midcell(umesh%ncell)    ! centres de cellule approches ( /= barycentre )

! -- OUTPUTS --
type(v3d), dimension(umesh%nface,2) :: cg_elem    ! centres de volume elementaire
real(krp), dimension(umesh%nface,2) :: vol_elem   ! volumes de volume elementaire
! -- Private DATA --
integer         :: if           ! indice  de face
integer         :: ic           ! indice  de cellule
real(krp)       :: kcg, kvol    ! coefficients pour le calcul du centre et du volume

! -- BODY --

! le volume elementaire est defini par une base (la face) de centre H
! et un sommet K (centre approche (midcell) de la cellule adjacente). 
! On en deduit le volume et le centre de gravite du volume elementaire.

select case(defmesh%geo)
case(geo_2d, geo_2daxi)
  kcg  = 1./3._krp
  kvol = 1./2._krp
case(geo_3d)
  kcg  = 1./4._krp
  kvol = 1./3._krp
case default
  call cfd_error("unknown geometry type (MESHGEOM%calc_ust_elemvol)")
endselect

do if = 1, umesh%nface
  ic = umesh%facecell%fils(if,1)
  !> @optim vectorize
  call subcalc_elemvol(midcell(ic), &
        umesh%mesh%face_normal(if,1), umesh%mesh%face_center(if,1), umesh%mesh%face_surf(if), &
        cg_elem(if,1), vol_elem(if,1))
  ic = umesh%facecell%fils(if,2)
  if ((ic > 0).and.(ic <= umesh%ncell_int)) then
    call subcalc_elemvol(midcell(ic), &
        umesh%mesh%face_normal(if,1), umesh%mesh%face_center(if,1), umesh%mesh%face_surf(if), &
        cg_elem(if,2), vol_elem(if,2))
  else
    cg_elem (if,2) = v3d(0._krp, 0._krp, 0._krp)
    vol_elem(if,2) = 0._krp
  endif
enddo

contains

  !---------------------------------------------------------------------------------
  subroutine subcalc_elemvol(vK, fn, fc, fs, cg, vol)
    implicit none
    ! --- Inputs ---
    type(v3d), intent(in) :: vK          ! coord. K (sommet) et H (centre de face) 
    type(v3d), intent(in) :: fn, fc      ! face normal and center !> @todo handle Gauss points 
    real(krp), intent(in) :: fs          ! face surface
    ! --- Outputs ---
    type(v3d), intent(out) :: cg          ! centre de volume elementaire
    real(krp), intent(out) :: vol         ! volume du volume elementaire
    ! --- interne ---
    type(v3d)       :: vKH         ! vecteur KH (sommet-centre de face)
    real(krp)       :: hauteur     ! volume du volume elementaire

      vKH     = fc - vK
      hauteur = abs(vKH.scal.fn)
      cg      = fc - kcg*vKH
      vol     = kvol*hauteur*fs

  endsubroutine subcalc_elemvol

endsubroutine calc_ust_elemvol

!------------------------------------------------------------------------------!
! Procedure : calc_ust_cell               Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  :
!   Calcul des cellules (centres et volumes) a partir des volumes
!   elementaires
!------------------------------------------------------------------------------!
subroutine calc_ust_cell(defmesh, umesh, cg_elem, vol_elem)
implicit none
! -- INPUTS/OUTPUTS --
type(mnu_mesh)      :: defmesh
type(st_ustmesh)    :: umesh                
! -- INPUTS --
type(v3d), dimension(umesh%nface,2) :: cg_elem    ! centres de volume elementaire
real(krp), dimension(umesh%nface,2) :: vol_elem   ! volumes de volume elementaire
! -- Private DATA --
integer         :: if           ! indice  de face
integer         :: ic, ic1, ic2 ! indices de cellules
real(krp)       :: two_pi

! -- BODY --

two_pi = 2._krp*PIcst

! les centres de gravite et les volumes des cellules elementaires sont
! indexes selon l'index de face. On doit utiliser la connectivite
! face->cellules pour faire la somme des volumes elementaires de chaque
! cellule.

do if = 1, umesh%nface
  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)
  ! --- premiere cellule connectee ---
  umesh%mesh%centre(ic1,1,1) = umesh%mesh%centre(ic1,1,1) + vol_elem(if,1)*cg_elem(if,1)
  umesh%mesh%volume(ic1,1,1) = umesh%mesh%volume(ic1,1,1) + vol_elem(if,1)
  ! --- seconde cellule connectee (si existante) ---
  if ((ic2 > 0).and.(ic2 <= umesh%ncell_int)) then
    umesh%mesh%centre(ic2,1,1) = umesh%mesh%centre(ic2,1,1) + vol_elem(if,2)*cg_elem(if,2)
    umesh%mesh%volume(ic2,1,1) = umesh%mesh%volume(ic2,1,1) + vol_elem(if,2)
  endif
enddo

! calcul de la moyenne sur les cellules
do ic = 1, umesh%ncell_int
  umesh%mesh%centre(ic,1,1) = umesh%mesh%centre(ic,1,1) / umesh%mesh%volume(ic,1,1)
enddo

select case(defmesh%geo)
case(geo_2d)
case(geo_2daxi)
  do if = 1, umesh%nface
    umesh%mesh%face_surf(if) = two_pi * umesh%mesh%face_center(if,1)%y * umesh%mesh%face_surf(if)
  enddo
  do ic = 1, umesh%ncell_int
    umesh%mesh%volume(ic,1,1) = two_pi * umesh%mesh%volume(ic,1,1) * umesh%mesh%centre(ic,1,1)%y
  enddo
case(geo_3d)
case default
  call cfd_error("unknown geometry type (MESHGEOM%calc_ust_elemvol)")
endselect


endsubroutine calc_ust_cell

!------------------------------------------------------------------------------!
! Procedure : calc_ust_checkface          Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  : see history
!   1. Face->cell connectivity is changed so that cell 1 index is lower than cell 2
!   2. Check face normals direction accordingg to the connectivity face->cell
!     (normal vector is oriented from cell 1 to cell 2)
!------------------------------------------------------------------------------!
subroutine calc_ust_checkface(facecell, mesh)
implicit none
! -- INPUTS --
type(st_connect)  :: facecell   ! connectivite face->cellules
! -- INPUTS/OUTPUTS --
type(st_mesh)        :: mesh       ! maillages
! -- Private DATA --
integer   :: if       ! index de face
integer   :: ic1, ic2 ! indices de cellules
type(v3d) :: v12      ! vecteur du centre de cellule 1 a centre de cellule 2

! -- BODY --

! -- change of connectivity indexes (except boundary faces where ic2 = 0) --

do if = 1, facecell%nbnodes
  if ((facecell%fils(if,2) /= 0).and.(facecell%fils(if,1) > facecell%fils(if,2))) then
    ic1                 = facecell%fils(if,1)
    facecell%fils(if,1) = facecell%fils(if,2)
    facecell%fils(if,2) = ic1
  endif
enddo

! -- check normal vector direction --

do if = 1, facecell%nbnodes     ! boucle sur les faces

  ic1 = facecell%fils(if,1)     ! index de cellules voisines a la face
  ic2 = facecell%fils(if,2)     ! par convention, la normale va de ic1 a ic2

  ! v12 est le vecteur du centre ic1 au centre ic2. 
  ! Si la face est limite, v12 est le centre ic1 vers le centre de la face

  if (ic2 /= 0) then
    v12 = mesh%centre(ic2,1,1) - mesh%centre(ic1,1,1)
  else
    v12 = mesh%face_center(if,1) - mesh%centre(ic1,1,1)
  endif

  ! si v12 et la normale sont inversees, on corrige la normale pour 
  ! etre en accord avec la convention des connectivites

  if ((v12.scal.mesh%face_normal(if,1)) < 0._krp) then
    mesh%face_normal(if,1) = - mesh%face_normal(if,1)
  endif

enddo

endsubroutine calc_ust_checkface
!------------------------------------------------------------------------------!
! Change history
!
! jan  2003 : creation de la procedure
! mai  2003 : correction de l'orientation des normales de faces limites
! july 2004 : change of connectivity indexes (from lower to upper)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
subroutine calc_meshgeom(defmesh, umesh)
! --- IN/OUTPUTS ---
type(mnu_mesh)       :: defmesh
type(st_ustmesh)     :: umesh
! -- Internal variables --
type(v3d), dimension(:),   allocatable :: midcell   ! centres approches de cellule
type(v3d), dimension(:,:), allocatable :: cg_elem   ! centres de volume elementaire
real(krp), dimension(:,:), allocatable :: vol_elem  ! volumes elementaires
! -- BODY --

allocate(midcell(umesh%ncell))
allocate(cg_elem(umesh%nface,2))
allocate(vol_elem(umesh%nface,2))

umesh%mesh%centre(1:umesh%ncell,1,1) = v3d_zero;
umesh%mesh%volume(1:umesh%ncell,1,1) = 0._krp

!---------------------------------------------------------------------------
! computation of face geometry (surface, normal, center)

call calc_ust_face(defmesh, umesh%facevtex, umesh%mesh)

!---------------------------------------------------------------------------
! estimate of cell centers (used to defined internal sub-cells) 

call calc_ust_midcell(umesh, midcell) 

!---------------------------------------------------------------------------
! computation of SUB-CELLS volumes and gravity centers

call calc_ust_elemvol(defmesh, umesh, midcell, cg_elem, vol_elem)
call calc_ust_cell   (defmesh, umesh, cg_elem, vol_elem)

deallocate(midcell)
deallocate(cg_elem)
deallocate(vol_elem)

call calc_ust_checkface(umesh%facecell, umesh%mesh)
call calc_mesh_info(umesh%mesh)

endsubroutine calc_meshgeom


endmodule MESHGEOM
!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2011: created from calc_ust_midcell, calc_ust_elemvol, calc_ust_cell, calc_ust_checkface
!------------------------------------------------------------------------------!



