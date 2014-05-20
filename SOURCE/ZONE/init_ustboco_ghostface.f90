!------------------------------------------------------------------------------!
! Procedure : init_ustboco_ghostface                 Authors : J. Gressier
!                                                    Created : March 2003
!> @brief
!>   Affectation des connectivites entre faces limites et cellules limites
!>   pour le type "ghostface" (point fictif sur la face) : cela revient a
!>   avoir une cellule fictive de volume nul.
!
!------------------------------------------------------------------------------!
subroutine init_ustboco_ghostface(ib, defboco, umesh)

use TYPHMAKE
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- Declaration des entrees --
integer        :: ib                     ! numero de condition aux limites
type(mnu_boco) :: defboco                ! parametres du solveur

! -- Declaration des entrees/sorties --
type(st_ustmesh) :: umesh                ! unstructured mesh

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: if                            ! indice boco et de face
integer :: icell, iface                  ! index de cellule et de face

! --- BODY ---

! affectation de connectivite face limites -> cellules fictives
! la variable umesh%ncell_lim contient le nombre courant de cellules limites affectees

! le tableau de cellules est cense pouvoir contenir le nombre de cellules fictives (test)

if ((umesh%ncell_lim+umesh%boco(ib)%nface)>(umesh%ncell-umesh%ncell_int)) then
  call error_stop("(Allocation) Pas assez de cellules allouees pour les cellules fictives")
endif

! -- boucle sur la liste des faces de la condition limite --

do if = 1, umesh%boco(ib)%nface    
  
  ! affectation de connectivite face limites -> cellules fictives
  umesh%ncell_lim = umesh%ncell_lim + 1     ! nouvelle cellule limite
  icell = umesh%ncell_int + umesh%ncell_lim ! index de cellule limite
  iface = umesh%boco(ib)%iface(if)          ! index de face

  ! definition geometrique de la cellule fictive
  umesh%mesh%volume(icell,1,1) = 0._krp
  umesh%mesh%centre(icell,1,1) = umesh%mesh%face_center(iface,1)
  if (umesh%facecell%fils(iface,2) == 0) then
    umesh%facecell%fils(iface,2) = icell    ! affectation de la cellule fictive
  else
    call error_stop("(Initialisation de connectivite) Connectivite deja affectee sur face limite")
  endif

enddo

endsubroutine init_ustboco_ghostface
!------------------------------------------------------------------------------!
! changes history
!
! mars 2003 : creation de la procedure
!------------------------------------------------------------------------------!
