!------------------------------------------------------------------------------!
! Procedure : fillmat_sing_effects        Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Remplissage de la matrice des coefficients d'effets des singularites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine fillmat_sing_effects(mat, size, ideb, grid, defsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use PAN2D_LIN

implicit none

! -- Declaration des entrees --
integer                         :: size, ideb
type(st_grid)                   :: grid
type(mnu_solver)                :: defsolver

! -- Declaration des sorties --
real(krp), dimension(size,size) :: mat

! -- Declaration des variables internes --
integer                :: ib, if, nf, ifg, iv1, iv2, irhs
type(st_grid), pointer :: pgrid
type(st_face)          :: face, pane
real(krp)              :: c1, c2
type(v3d)              :: x1, x2

! -- Debut de la procedure --

! si remplissage par ligne   : effet de toutes les singularites en un point fixe
! si remplissage par colonne : effet d'une seule singularite en chaque pt (centre)
! (test d'efficacite CPU max ?)

!print*,'mat fillmat', real(mat,4)
! -- remplissage par ligne --

do ifg = 1, grid%umesh%nface    ! boucle sur les faces de la grille locale

  ! ecriture de la condition limite V.n = 0
  face = grid%umesh%mesh%iface(ifg, 1, 1)
  irhs = 1
  !print*,'fillmat0:',face%centre, face%normale

  do ib = 1, defsolver%nboco  ! boucle sur boco / rech. de cond. singularites
    if (defsolver%boco(ib)%typ_calc == bc_calc_singpanel) then
    
      !print*,'boco',ib, ifg, ideb, irhs
      pgrid => defsolver%boco(ib)%boco_vortex%pgrid
      !! DEV : memorisation de l'indice dans la structure boco_vortex
      nf = pgrid%umesh%nface

      do if = 1, nf   ! boucle sur tous les panneaux singularites
        !pane = pgrid%umesh%mesh%iface(if, 1, 1)
        iv1  = pgrid%umesh%facevtex%fils(if,1)
        iv2  = pgrid%umesh%facevtex%fils(if,2)
        x1   = pgrid%umesh%mesh%vertex(iv1, 1, 1)
        x2   = pgrid%umesh%mesh%vertex(iv2, 1, 1)
        call coef_induc_pvortlin2d(face%centre-.00001_krp*face%normale, face%normale, x1, x2, c1, c2)
        !print*,'fillmat1:',iv1, iv2, real( (/c1, c2/), 4)
        !print*,'fillmat2:',real( (/x1%x, x1%y, x2%x, x2%y /), 4)
        mat(ideb+ifg-1, irhs-1+iv1) = mat(ideb+ifg-1, irhs-1+iv1) + c1
        mat(ideb+ifg-1, irhs-1+iv2) = mat(ideb+ifg-1, irhs-1+iv2) + c2
      enddo
 
      irhs = irhs + nf+1

    endif
  enddo

enddo


!-----------------------------
endsubroutine fillmat_sing_effects

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : creation de la procedure
!------------------------------------------------------------------------------!
