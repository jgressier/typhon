!------------------------------------------------------------------------------!
! Procedure : calc_connface               Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Calcul de connectivités face/face entre deux zones selon la liste de
!   faces de famille pour chacune des zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_connface(m1, b1, m2, b2)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrées --
type(st_ustmesh) :: m1, m2      ! maillage 1 et 2 connectées (non structurés)
type(st_ustboco) :: b1, b2      ! conditions aux limites concernées par la connection

! -- Declaration des entrées/sorties --
!type(st_ustmesh) :: ust_mesh

! -- Declaration des sorties --


! -- Declaration des variables internes --
integer                              :: npts1, npts2        ! nb de points par zone/famille
type(v3d), dimension(:), allocatable :: lst_pts1, lst_pts2  ! liste des sommets extraits
integer,   dimension(:), allocatable :: ind_pts1, ind_pts2  ! index des sommets por extraction
!integer                 :: ic, ic1, ic2 ! indices de cellules

! -- Debut de la procedure --


! création des listes de points associés aux familles de conditions aux limites

allocate(ind_pts1(m1%nvtex))
allocate(ind_pts2(m2%nvtex))

call extract_pts_index(m1, b1, npts1, ind_pts1)
call extract_pts_index(m2, b2, npts2, ind_pts2)

! création des listes de points pour chaque zone

allocate(lst_pts1(npts1))
allocate(lst_pts2(npts2))

call extract_points(m1, ind_pts1, lst_pts1)
call extract_points(m2, ind_pts2, lst_pts2)

!
! création des listes de faces avec la nouvelle renumérotation des points


! création de la connectivité des points entre zones


! destruction des tableaux provisoires

deallocate(ind_pts1, ind_pts2)
deallocate(lst_pts1, lst_pts2)


endsubroutine calc_connface

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juin 2003 (v0.0.1b): création de la procédure
! 
!------------------------------------------------------------------------------!
