!------------------------------------------------------------------------------!
! Procedure : calc_induced_velocities     Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Calcul des vitesses induites dans une zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_induced_velocities(defsolver, pos, vel, dim)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use PAN2D_LIN

implicit none

! -- Declaration des entrées --
integer                     :: dim
type(mnu_solver)            :: defsolver
type(v3d), dimension(1:dim) :: pos

! -- Declaration des sorties --
type(v3d), dimension(1:dim) :: vel

! -- Declaration des variables internes --
integer                :: ib, if, nf, ipos, iv1, iv2
type(st_grid), pointer :: pgrid
type(st_face)          :: pane
real(krp)              :: c1, c2
type(v3d)              :: x1, x2

! -- Debut de la procedure --

! initialisation

do if = 1, dim
  vel(if) = v3d(0._krp, 0._krp, 0._krp)
enddo

do ib = 1, defsolver%nboco  ! boucle sur boco / rech. de cond. singularités

  select case(defsolver%boco(ib)%typ_calc)

  case(bc_calc_kutta)

  case(bc_calc_farfield)

    ! DEV : créer les opérations sur les tableaux de vecteurs
    do if = 1, dim
      vel(if) = vel(if) + defsolver%boco(ib)%boco_vortex%vect
      !vel(1:dim) = vel(1:dim) + defsolver%boco(ib)%boco_vortex%vect
    enddo

  case(bc_calc_singpanel)

    pgrid => defsolver%boco(ib)%boco_vortex%pgrid
    nf    =  pgrid%umesh%nface

    do if = 1, nf   ! boucle sur tous les panneaux singularités
      pane = pgrid%umesh%mesh%iface(if, 1, 1)
      iv1  = pgrid%umesh%facevtex%fils(if,1)
      iv2  = pgrid%umesh%facevtex%fils(if,2)
      x1   = pgrid%umesh%mesh%vertex(iv1, 1, 1)
      x2   = pgrid%umesh%mesh%vertex(iv2, 1, 1)
      c1   = pgrid%bocofield%tabscal(1)%scal(iv1)
      c2   = pgrid%bocofield%tabscal(1)%scal(iv2)

      do ipos = 1, dim
        ! DEV : généralisation au calcul d'un tableau
        vel(ipos) = vel(ipos) + vel_induc_pvortlin2d(pos(ipos), x1, x2, c1, c2)
      enddo

    enddo ! boucle sur les panneaux
 
  endselect

enddo ! boucle sur les BC

!do ipos = 1, dim
!print*,'vel:',real((/vel(ipos)%x,vel(ipos)%y, vel(ipos)%y/vel(ipos)%x/),4)
!enddo

!-----------------------------
endsubroutine calc_induced_velocities

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : création de la procédure
!------------------------------------------------------------------------------!
