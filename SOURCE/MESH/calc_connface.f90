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
subroutine calc_connface(m1, b1, connface1, m2, b2, connface2)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --
type(st_ustmesh) :: m1, m2      ! maillage 1 et 2 connectees (non structures)
type(st_ustboco) :: b1, b2      ! conditions aux limites concernees par la connection

! -- Declaration des entrees/sorties --
integer, dimension(1:b1%nface) :: connface1
integer, dimension(1:b2%nface) :: connface2

! -- Declaration des sorties --


! -- Declaration des variables internes --
type(v3d), dimension(:), allocatable :: centre1, centre2
real(krp)                            :: mincentre
integer                              :: if1, if2, ind_assoc

! -- Debut de la procedure --

allocate(centre1(b1%nface))
allocate(centre2(b2%nface))

! creation de la liste des centres des faces concernees des deux zones
call extract_centre(b1, m1, centre1)
call extract_centre(b2, m2, centre2)

! calcul pour chaque centre de la zone 1 du centre le plus proche de la zone 2
! et affectation des indices aux connectivites de faces
do if1 = 1, b1%nface
  mincentre = abs( centre1(if1) - centre2(1) )
  ind_assoc = 1
  if ( b2%nface .gt. 1 ) then
    do if2 = 2, b2%nface
      if ( abs( centre1(if1) - centre2(if2) ) .lt. mincentre) then
        mincentre = abs( centre1(if1) - centre2(if2) )
        ind_assoc = if2
      endif
    enddo
  endif
  connface1(if1) = ind_assoc
  connface2(ind_assoc) = if1

enddo

deallocate(centre1, centre2)

endsubroutine calc_connface

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juin 2003 (v0.0.1b): creation de la procedure
! Fevrier 2004       : connectivites determinees par la coincidence des centres
!                      de faces
! 
!------------------------------------------------------------------------------!
