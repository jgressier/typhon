!------------------------------------------------------------------------------!
! Procedure : output_tec_usti_node        Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : cf Historique
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format TECPLOT
!   Valeurs aux noeuds du maillage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_tec_ust_node(uf, ust_mesh, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrées --
integer          :: uf            ! unité d'écriture
type(st_ustmesh) :: ust_mesh      ! maillage à écrire
type(st_field)   :: field         ! champ de valeurs

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer                :: i, if, ic
integer                :: info
type(v3d)              :: vtex
type(st_genericfield)  :: vtexfield
type(st_cellvtex)      :: cellvtex
character(len=30)      :: sformat

real(krp) :: a, b, L, T0, T1, alpha, beta, temp

! -- Debut de la procedure --

! -- Calcul de la connectivité CELL->VTEX --

call calc_cellvtex(ust_mesh%nbdim, cellvtex, ust_mesh%ncell, ust_mesh%ncell_int, &
                   ust_mesh%facecell, ust_mesh%facevtex)

call verify_cellvtex(ust_mesh%mesh, cellvtex)

! -- Entete de fichier ---

write(uf_chpresu,*) 'ZONE T="USTMESH", F=FEPOINT, N=',ust_mesh%nvtex, &
                    ',E=',cellvtex%nquad,',ET=QUADRILATERAL'

! -- Calcul des valeurs aux sommets --

call new(vtexfield, ust_mesh%nvtex, field%etatprim%nscal, field%etatprim%nvect, 0)
!!allocate(nsum(ust_mesh%nvtex))

call interpol_onvtex(0, cellvtex, field%etatprim, vtexfield)

do i = 1, vtexfield%dim
  vtex = ust_mesh%mesh%vertex(i,1,1)
  write(uf_chpresu,'(4e18.8)') vtex%x, vtex%y, vtex%z, vtexfield%tabscal(1)%scal(i)
enddo

! -- Ecriture de la connectivité --

!!! UNIQUEMENT LES QUAD DANS CETTE VERSION

!write(sformat,*) '(i8)'
do i = 1, cellvtex%nquad
  write(uf_chpresu, '(4i8)') cellvtex%quad%fils(i,1:4)
enddo

! desallocation

call delete(vtexfield)
call delete(cellvtex)
!!deallocate(nsum)


endsubroutine output_tec_ust_node
