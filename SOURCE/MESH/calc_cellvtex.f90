!------------------------------------------------------------------------------!
! Procedure : calc_cellvtex               Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul de connectivit�s cell->vtex � partir de connectivit�s
!   face->cell et face->vtex
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_cellvtex(ndim, cellvtex, ncell, ncell_int, facecell, facevtex)

use TYPHMAKE
use CONNECTIVITY
use USTMESH
use STRING

implicit none

! -- Declaration des entr�es --
integer          :: ndim                  ! mode 2D ou mode 3D
type(st_connect) :: facecell, facevtex    ! connectivit�s en entr�es
integer          :: ncell                 ! nombre total de cellules 
integer          :: ncell_int             ! nombre de cellules internes
                                          !   ncell-ncell_int est le nombre de
                                          !   de cellules limites

! -- Declaration des sorties --
type(st_cellvtex) :: cellvtex

! -- Declaration des variables internes --
type(st_connect)     :: xcellvtex    ! tableau interm�diaire des connectivit�s
integer, allocatable :: nvtex(:)     ! nombre de sommet par �l�ment
integer              :: if, ic, iv   ! index de face, cellule, et sommet
integer              :: vtex         ! num�ro de sommet
integer              :: info         

! -- Debut de la procedure --

! cr�ation d'une connectivit� cell->vertex provisoire

call new(xcellvtex, ncell, 8)        ! une cellule est cens�e avoir 8 sommets max
allocate(nvtex(ncell))               ! nombre r�el de sommets par �l�ment
nvtex(:) = 0

! remplissage de la connectivit� avec test de doublons de vertex

do if = 1, facecell%nbnodes          ! boucle sur les faces
  do iv = 1, facevtex%nbfils         ! boucle sur les sommets de chaque face
    vtex = facevtex%fils(if,iv)
    if (vtex /= 0) then
      ! premi�re cellule voisine de la voisine
      ic = facecell%fils(if,1)
      if (nvtex(ic) /= 0) then
        if (index(vtex, xcellvtex%fils(ic,1:nvtex(ic))) /= 0) then
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
        if (index(vtex, xcellvtex%fils(ic,1:nvtex(ic))) /= 0) then
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

! --- transformation connectivit� originale en connectivit� adapt�e CELLVTEX ---

call init(cellvtex)
cellvtex%dim = ndim

! on compte le nombre d'�l�ments pour d'abord faire les allocation

info = 0

select case(ndim)
case(2)
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
case(3)
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

if (info/=0) call erreur("Erreur dans le calcul de connectivit� cell/vtex",&
                         "code erreur"//strof(info,10))

!!! ATTENTION : l'ordre des vertex peut �tre important pour la d�finition de l'�lement
!!! c'est surtout le cas en 3D pour les PYRA, PENTA et HEXA

! --- correction des cellules limites qui sont souvent d�g�n�r�es en des faces ---

do ic = ncell_int+1, ncell
  select case(nvtex(ic))

    case(2)
      ! on est cens� avoir un maillage 2D (QUAD ou TRI) et l'�l�ment limite est un BAR
      if (cellvtex%nquad /= 0) then
        ! on transforme le BAR en QUAD (ordre CGNS des sommets)
        nvtex(ic) = 4
        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,2) 
        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
      else
        ! on transforme le BAR en TRI
        nvtex(ic) = 3
        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,1) 
      endif

    case(3)
      ! on est cens� avoir un maillage 3D (TETRA, PYRA ou PENTA) 
      ! et l'�l�ment limite est un TRI
      if (cellvtex%ntetra /= 0) then
        ! on transforme le TRI en TETRA
        nvtex(ic) = 4
        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
      else if (cellvtex%npenta /= 0) then
        ! on transforme le TRI en PENTA (ordre CGNS des sommets)
        nvtex(ic) = 6
        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,2) 
        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,3) 
      else ! CAS EXCEPTIONNEL
        ! on transforme le TRI en PYRA (ordre CGNS des sommets)
        nvtex(ic) = 5
        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,3)   ! �change sommets 3 et 5 
        xcellvtex%fils(ic,3) = xcellvtex%fils(ic,2) 
        xcellvtex%fils(ic,4) = xcellvtex%fils(ic,1) 
      endif

    case(4)
      ! on est cens� avoir un maillage 3D (PYRA, PENTA, ou HEXA) 
      ! et l'�l�ment limite est un QUAD
      ! Attention : si ce n'est pas une face, cela peut-�tre un TETRA
      if (cellvtex%nhexa /= 0) then
        ! on transforme le QUAD en HEXA (ordre CGNS des sommets)
        nvtex(ic) = 8
        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,1) 
        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,2) 
        xcellvtex%fils(ic,7) = xcellvtex%fils(ic,3) 
        xcellvtex%fils(ic,8) = xcellvtex%fils(ic,4) 
      else if (cellvtex%npenta /= 0) then
        ! on transforme le QUAD en PENTA (ordre CGNS des sommets)
        nvtex(ic) = 6
        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,3) 
        xcellvtex%fils(ic,6) = xcellvtex%fils(ic,4) 
      else  ! CAS EXCEPTIONNEL
        ! on transforme le QUAD en PYRA (ordre CGNS des sommets)
        nvtex(ic) = 5
        xcellvtex%fils(ic,5) = xcellvtex%fils(ic,1)
      endif

    endselect
enddo

! allocation

call new(cellvtex)

! Copie des connectivit�s

call init_cellvtex(cellvtex)   ! remize � z�ro des compteurs

select case(ndim)
case(2)
  do ic = 1, ncell
    select case(nvtex(ic))
    case(2)
      cellvtex%nbar  = cellvtex%nbar + 1
      cellvtex%bar%fils(cellvtex%nbar,:) = xcellvtex%fils(ic, 1:2)
    case(3)
      cellvtex%ntri  = cellvtex%ntri + 1
      cellvtex%bar%fils(cellvtex%ntri,:) = xcellvtex%fils(ic, 1:3)
    case(4)
      cellvtex%nquad = cellvtex%nquad + 1
      cellvtex%bar%fils(cellvtex%nquad,:) = xcellvtex%fils(ic, 1:4)
    endselect
  enddo
case(3)
  do ic = 1, ncell
    select case(nvtex(ic))
    case(4)
      cellvtex%ntetra = cellvtex%ntetra + 1
      cellvtex%bar%fils(cellvtex%ntetra,:) = xcellvtex%fils(ic, 1:4)
    case(5)
      cellvtex%npyra  = cellvtex%npyra  + 1
      cellvtex%bar%fils(cellvtex%npyra,:) = xcellvtex%fils(ic, 1:5)
    case(6)
      cellvtex%npenta = cellvtex%npenta + 1
      cellvtex%bar%fils(cellvtex%npenta,:) = xcellvtex%fils(ic, 1:6)
    case(8)
      cellvtex%nhexa  = cellvtex%nhexa  + 1
      cellvtex%bar%fils(cellvtex%nhexa,:) = xcellvtex%fils(ic, 1:8)
    endselect
  enddo
endselect

deallocate(nvtex)
call delete(xcellvtex)


endsubroutine calc_cellvtex

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juil 2003 : cr�ation de la proc�dure
! 
!------------------------------------------------------------------------------!
