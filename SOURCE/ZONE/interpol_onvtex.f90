!------------------------------------------------------------------------------!
! Procedure : interpol_onvtex             Auteur : J. Gressier
!                                         Date   : Aout 2003
! Fonction                                Modif  : (cf historique)
!   Interpolation d'un champ de cellules sur les sommets de la cellule
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine interpol_onvtex(type_interpol, cellvtex, cellfield, vtexfield)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
integer               :: type_interpol    ! choix du type de calcul
type(st_cellvtex)     :: cellvtex         ! connectivite cell->vertex
type(st_genericfield) :: cellfield        ! champ des cellules

! -- Declaration des sorties --
type(st_genericfield) :: vtexfield        ! champ des sommets a calculer

! -- Declaration des variables internes --
integer, allocatable :: ncell(:)          ! nombre de cellules sommees par sommet
integer              :: i, ic, iv, ivtex, isca, ivec

! -- Debut de la procedure --

!!! DANS CETTE VERSION, ON NE PROPOSE QUE LA MOYENNE DES ETATS DES 
!!! CELLULES SUR LE SOMMET COMMUN

allocate(ncell(vtexfield%dim))
ncell(:) = 0

call init_genericfield(vtexfield, 0._krp, v3d(0._krp, 0._krp, 0._krp))

! --- Somme sur les sommets (boucle sur les cellules) ---

if (cellvtex%nbar /= 0) then
  do i = 1, cellvtex%nbar
    ic = cellvtex%ibar(i)
    do iv = 1, 2
      ivtex        = cellvtex%bar%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%ntri /= 0) then
  do i = 1, cellvtex%ntri
    ic = cellvtex%itri(i)
    do iv = 1, 3
      ivtex        = cellvtex%tri%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%nquad /= 0) then
  do i = 1, cellvtex%nquad
    ic = cellvtex%iquad(i)
    do iv = 1, 4
      ivtex        = cellvtex%quad%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%ntetra /= 0) then
  do i = 1, cellvtex%ntetra
    ic = cellvtex%itetra(i)
    do iv = 1, 4
      ivtex        = cellvtex%tetra%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%npyra /= 0) then
  do i = 1, cellvtex%npyra
    ic = cellvtex%ipyra(i)
    do iv = 1, 5
      ivtex        = cellvtex%pyra%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%npenta /= 0) then
  do i = 1, cellvtex%npenta
    ic = cellvtex%ipenta(i)
    do iv = 1, 6
      ivtex        = cellvtex%penta%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif

if (cellvtex%nhexa /= 0) then
  do i = 1, cellvtex%nhexa
    ic = cellvtex%ihexa(i)
    do iv = 1, 8
      ivtex        = cellvtex%hexa%fils(i,iv)
      ncell(ivtex) = ncell(ivtex) + 1
      do isca = 1, cellfield%nscal
        vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) &
                                            + cellfield%tabscal(isca)%scal(ic) 
      enddo
      do ivec = 1, cellfield%nvect
        vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) &
                                            + cellfield%tabvect(ivec)%vect(ic) 
      enddo
    enddo
  enddo
endif


! --- Calcul des moyennes par la division du nombre de termes sommes ---

do ivtex = 1, vtexfield%dim
  do isca = 1, vtexfield%nscal
    vtexfield%tabscal(isca)%scal(ivtex) = vtexfield%tabscal(isca)%scal(ivtex) / ncell(ivtex)
  enddo
  do ivec = 1, vtexfield%nvect
    vtexfield%tabvect(ivec)%vect(ivtex) = vtexfield%tabvect(ivec)%vect(ivtex) / real(ncell(ivtex),krp)
  enddo
enddo 


deallocate(ncell)


!-----------------------------
endsubroutine interpol_onvtex

!------------------------------------------------------------------------------!
! Historique des modifications
!
! aout 2003 : creation de la procedure
!------------------------------------------------------------------------------!
