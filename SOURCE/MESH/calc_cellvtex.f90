!------------------------------------------------------------------------------!
! Procedure : calc_cellvtex               Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul de connectivites cell->vtex a partir de connectivites
!   face->cell et face->vtex
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_cellvtex(geom, cellvtex, ncell, ncell_int, facecell, facevtex)

use TYPHMAKE
use CONNECTIVITY
use USTMESH
use STRING

implicit none

! -- Declaration des entrees --
character        :: geom                  ! mode geometrique
type(st_connect) :: facecell, facevtex    ! connectivites en entrees
integer          :: ncell                 ! nombre total de cellules 
integer          :: ncell_int             ! nombre de cellules internes
                                          !   ncell-ncell_int est le nombre de
                                          !   de cellules limites

! -- Declaration des sorties --
type(st_cellvtex) :: cellvtex

! -- Declaration des variables internes --
type(st_connect)     :: xcellvtex    ! tableau intermediaire des connectivites
integer, allocatable :: nvtex(:)     ! nombre de sommet par element
integer              :: if, ic, iv   ! index de face, cellule, et sommet
integer              :: vtex         ! numero de sommet
integer              :: info         

! -- Debut de la procedure --

! creation d'une connectivite cell->vertex provisoire

call new(xcellvtex, ncell, 8)        ! une cellule est censee avoir 8 sommets max
allocate(nvtex(ncell))               ! nombre reel de sommets par element
nvtex(:) = 0

! remplissage de la connectivite avec test de doublons de vertex

do if = 1, facecell%nbnodes          ! boucle sur les faces
  do iv = 1, facevtex%nbfils         ! boucle sur les sommets de chaque face
    vtex = facevtex%fils(if,iv)
    if (vtex /= 0) then              ! si le sommet existe
      ! premiere cellule voisine de la voisine
      ic = facecell%fils(if,1)
      if (nvtex(ic) /= 0) then       ! si il y a deja des sommets affectes
        ! on recherche si le sommet courant a deja ete affecte
        if (index(vtex, xcellvtex%fils(ic,1:nvtex(ic))) == 0) then
          nvtex(ic)                    = nvtex(ic) + 1
          xcellvtex%fils(ic,nvtex(ic)) = vtex
        endif 
      else
        nvtex(ic)                    = nvtex(ic) + 1
        xcellvtex%fils(ic,nvtex(ic)) = vtex
      endif
      ! seconde cellule voisine de la voisine
      ic = facecell%fils(if,2)
      if (nvtex(ic) /= 0) then
        if (index(vtex, xcellvtex%fils(ic,1:nvtex(ic))) == 0) then
          nvtex(ic)                    = nvtex(ic) + 1
          xcellvtex%fils(ic,nvtex(ic)) = vtex
        endif 
      else
        nvtex(ic)                    = nvtex(ic) + 1
        xcellvtex%fils(ic,nvtex(ic)) = vtex
      endif
    endif
  enddo
enddo

!!! ATTENTION : l'ordre des vertex peut etre important pour la definition de l'element
!!! c'est surtout le cas en 3D pour les PYRA, PENTA et HEXA mais aussi les QUAD
! a faire en externe a cette routine car on a besoin des coordonnees des points.

!!! DANS UNE VERSION FUTURE, il sera utile de regarder le type des faces pour construire
!!! la cellule et reprendre les sommets dans l'ordre voulu.

! --- correction des cellules limites qui sont souvent degenerees en des faces ---
!
!do ic = ncell_int+1, ncell
!  select case(nvtex(ic))
!
!    case(2)
!      ! on est cense avoir un maillage 2D (QUAD ou TRI) et l'element limite est un BAR
!      if (cellvtex%nquad /= 0) then
!        ! on transforme le BAR en QUAD (ordre CGNS des sommets)
!        nvtex(ic) = 4
!        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,2) 
!        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
!      else
!        ! on transforme le BAR en TRI
!        nvtex(ic) = 3
!        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,1) 
!      endif
!
!    case(3)
!      ! on est cense avoir un maillage 3D (TETRA, PYRA ou PENTA) 
!      ! et l'element limite est un TRI
!      if (cellvtex%ntetra /= 0) then
!        ! on transforme le TRI en TETRA
!        nvtex(ic) = 4
!        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
!      else if (cellvtex%npenta /= 0) then
!        ! on transforme le TRI en PENTA (ordre CGNS des sommets)
!        nvtex(ic) = 6
!        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
!        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,2) 
!        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,3) 
!      else ! CAS EXCEPTIONNEL
!        ! on transforme le TRI en PYRA (ordre CGNS des sommets)
!        nvtex(ic) = 5
!        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,3)   ! echange sommets 3 et 5 
!        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,2) 
!        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
!      endif

!    case(4)
!      ! on est cense avoir un maillage 3D (PYRA, PENTA, ou HEXA) 
!      ! et l'element limite est un QUAD
!      ! Attention : si ce n'est pas une face, cela peut-etre un TETRA
!      if (cellvtex%nhexa /= 0) then
!        ! on transforme le QUAD en HEXA (ordre CGNS des sommets)
!        nvtex(ic) = 8
!        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,1) 
!        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,2) 
!        xcellvtex%fils(ic,7) = xcellvtex%fils(ic,3) 
!        xcellvtex%fils(ic,8) = xcellvtex%fils(ic,4) 
!      else if (cellvtex%npenta /= 0) then
!        ! on transforme le QUAD en PENTA (ordre CGNS des sommets)
!        nvtex(ic) = 6
!        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,3) 
!        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,4) 
!      else  ! CAS EXCEPTIONNEL
!        ! on transforme le QUAD en PYRA (ordre CGNS des sommets)
!        nvtex(ic) = 5
!        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,1)
!      endif

!    endselect
!enddo

! --- transformation connectivite originale en connectivite adaptee CELLVTEX ---

call init(cellvtex)

! on compte le nombre d'elements pour d'abord faire les allocations

info = 0

select case(geom)
case(msh_2dplan)
  cellvtex%dim = 2
  do ic = 1, ncell
    select case(nvtex(ic))
    case(2)
      cellvtex%nbar  = cellvtex%nbar  + 1
    case(3)
      cellvtex%ntri  = cellvtex%ntri  + 1
    case(4)
      cellvtex%nquad = cellvtex%nquad + 1
    case default
      info = ic
    endselect
  enddo
case(msh_3d)
  cellvtex%dim = 3
  do ic = 1, ncell
    select case(nvtex(ic))
    case(4)
      cellvtex%ntetra = cellvtex%ntetra + 1
    case(5)
      cellvtex%npyra  = cellvtex%npyra  + 1
    case(6)
      cellvtex%npenta = cellvtex%npenta + 1
    case(8)
      cellvtex%nhexa  = cellvtex%nhexa  + 1
    case default
      info = ic
    endselect
  enddo
case default
  info = -1
endselect

if (info/=0) call erreur("dans le calcul de connectivite cell/vtex",      &
                         "code erreur "//trim(adjustl(strof(info,10)))//  &
                         " ("//trim(adjustl(strof(nvtex(info),2)))//")")
! allocation

call new(cellvtex)

! Copie des connectivites

call init_cellvtex(cellvtex)   ! remize a zero des compteurs

select case(cellvtex%dim)
case(2)
  do ic = 1, ncell
    select case(nvtex(ic))
    case(2)
      cellvtex%nbar  = cellvtex%nbar + 1
      cellvtex%bar%fils(cellvtex%nbar,:) = xcellvtex%fils(ic, 1:2)
      cellvtex%ibar(cellvtex%nbar) = ic
    case(3)
      cellvtex%ntri  = cellvtex%ntri + 1
      cellvtex%tri%fils(cellvtex%ntri,:) = xcellvtex%fils(ic, 1:3)
      cellvtex%itri(cellvtex%ntri) = ic
    case(4)
      cellvtex%nquad = cellvtex%nquad + 1
      cellvtex%quad%fils(cellvtex%nquad,:) = xcellvtex%fils(ic, 1:4)
      cellvtex%iquad(cellvtex%nquad) = ic
    endselect
  enddo
case(3)
  do ic = 1, ncell    !!! ATTENTION, on peut avoir des CELLULES degenerees en FACES en 3D
    select case(nvtex(ic))
    case(4)
      cellvtex%ntetra = cellvtex%ntetra + 1
      cellvtex%tetra%fils(cellvtex%ntetra,:) = xcellvtex%fils(ic, 1:4)
      cellvtex%itetra(cellvtex%ntetra) = ic
    case(5)
      cellvtex%npyra  = cellvtex%npyra  + 1
      cellvtex%pyra%fils(cellvtex%npyra,:) = xcellvtex%fils(ic, 1:5)
      cellvtex%ipyra(cellvtex%npyra) = ic
    case(6)
      cellvtex%npenta = cellvtex%npenta + 1
      cellvtex%penta%fils(cellvtex%npenta,:) = xcellvtex%fils(ic, 1:6)
      cellvtex%ipenta(cellvtex%npenta) = ic
    case(8)
      cellvtex%nhexa  = cellvtex%nhexa  + 1
      cellvtex%hexa%fils(cellvtex%nhexa,:) = xcellvtex%fils(ic, 1:8)
      cellvtex%ihexa(cellvtex%nhexa) = ic
    endselect
  enddo
endselect

deallocate(nvtex)
call delete(xcellvtex)


endsubroutine calc_cellvtex

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juil 2003 : creation de la procedure
! 
!------------------------------------------------------------------------------!
