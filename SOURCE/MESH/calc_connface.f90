!------------------------------------------------------------------------------!
! Procedure : calc_connface               Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Calcul de connectivites face/face entre deux zones selon la liste de
!   faces de famille pour chacune des zones
!
! Defauts/Limitations/Divers : maillages coincidents
!
!------------------------------------------------------------------------------!
subroutine calc_connface(umesh1, boco1, connface1, umesh2, boco2, connface2)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH
use GRID_CONNECT

implicit none

! -- INPUTS --
type(st_ustmesh), intent(in) :: umesh1, umesh2      ! maillage 1 et 2 connectees (non structures)
type(st_ustboco), intent(in)  :: boco1, boco2      ! conditions aux limites concernees par la connection

! -- OUTPUTS --
integer, dimension(1:boco1%nface) :: connface1
integer, dimension(1:boco2%nface) :: connface2

! --- Private data ---
type(v3d), dimension(:), allocatable :: centre1, centre2
real(krp)                            :: mincentre
integer                              :: if1, if2, ind_assoc

! -- BODY --

allocate(centre1(boco1%nface))
allocate(centre2(boco2%nface))

! creation de la liste des centres des faces concernees des deux zones
call get_bocofacecenter(boco1, umesh1, centre1)
call get_bocofacecenter(boco2, umesh2, centre2)

call matching_index(centre1, centre2, connface1, connface2)

deallocate(centre1, centre2)

endsubroutine calc_connface

!------------------------------------------------------------------------------!
! Changes history
!
! June 2003: creation
! Feb  2004: connectivites determinees par la coincidence des centres  de faces
! Sept 2008: transfer matching algorithm to GRID_CONNECT module
!------------------------------------------------------------------------------!
