!------------------------------------------------------------------------------!
! Procedure : flux_to_res                 Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Calcul de residu a partir des flux
!
! Defauts/Limitations/Divers :
!   ATTENTION : le residu en sortie est different selon le traitement implicite
!               ou non
!
!------------------------------------------------------------------------------!
subroutine flux_to_res(dt, umesh, flux, residu, trait_jac, jacL, jacR)

use TYPHMAKE
!use GEO3D
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
real(krp)             :: dt         ! pas de temps CFL
type(st_ustmesh)      :: umesh      ! domaine non structure a integrer
type(st_genericfield) :: flux       ! tableaux des flux
logical               :: trait_jac  ! choix de traitement des jacobiennes

! -- Declaration des sorties --
type(st_genericfield)   :: residu            ! champ de residus
real(krp), dimension(1:umesh%nface) :: jacL, jacR 

! -- Declaration des variables internes --
real(krp), allocatable :: surf(:)         ! intermediate surface
real(krp), allocatable :: vol(:)          ! intermediate volume
integer,   allocatable :: tic1(:), tic2(:)  ! intermediate index connectivity
integer               :: if               ! index de face
integer               :: ic1, ic2         ! index de cellules
integer               :: ip               ! index de variables
integer               :: ib               ! index de conditions aux limites
integer               :: i                ! index de face
integer               :: ic               ! index de couplage

! -- Debut de la procedure --

allocate(surf(umesh%nface))

do if = 1, umesh%nface
  surf(if) = umesh%mesh%iface(if,1,1)%surface
enddo

! -- flux surfaciques -> flux de surfaces --

do ip = 1, flux%nscal
  flux%tabscal(ip)%scal(:) = surf(:) * flux%tabscal(ip)%scal(:)
enddo
do ip = 1, flux%nvect
  call scaleval(flux%tabvect(ip)%vect, surf(:))
enddo

! -- idem traitement des jacobiennes

if (trait_jac) then
  jacL(1:umesh%nface) = surf(:) * jacL(1:umesh%nface)
  jacR(1:umesh%nface) = surf(:) * jacR(1:umesh%nface)
endif

deallocate(surf)

! -- calcul des residus --

call init_genericfield(residu, 0._krp, v3d(0._krp, 0._krp, 0._krp))

!allocate(tic1(umesh%nface))
!allocate(tic2(umesh%nface))

!tic1 = umesh%facecell%fils(1:umesh%nface,1)
!tic2 = umesh%facecell%fils(1:umesh%nface,2)

do ip = 1, residu%nscal
  do if = 1, umesh%nface
    ic1 = umesh%facecell%fils(if,1)
    ic2 = umesh%facecell%fils(if,2)
    residu%tabscal(ip)%scal(ic1) = residu%tabscal(ip)%scal(ic1) - flux%tabscal(ip)%scal(if)
    residu%tabscal(ip)%scal(ic2) = residu%tabscal(ip)%scal(ic2) + flux%tabscal(ip)%scal(if)
  enddo
enddo

do ip = 1, residu%nvect
  do if = 1, umesh%nface
    ic1 = umesh%facecell%fils(if,1)
    ic2 = umesh%facecell%fils(if,2)
    call shiftopp(residu%tabvect(ip)%vect(ic1), flux%tabvect(ip)%vect(if))
    call shiftval(residu%tabvect(ip)%vect(ic2), flux%tabvect(ip)%vect(if))
  enddo
enddo

!deallocate(tic1, tic2)
! ??? creation de procedure intrinseques ? // optimisation

if (.not.trait_jac) then
 
  do ic1 = 1, umesh%ncell_int
    do ip = 1, residu%nscal
      residu%tabscal(ip)%scal(ic1) = dt * residu%tabscal(ip)%scal(ic1) &
                                     / umesh%mesh%volume(ic1,1,1)
    enddo
    do ip = 1, residu%nvect
      residu%tabvect(ip)%vect(ic1) = dt * residu%tabvect(ip)%vect(ic1)  &
                                     / umesh%mesh%volume(ic1,1,1)
    enddo
  enddo
endif


endsubroutine flux_to_res

!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : created
! dec  2004 : optimization (surface multiplication)
!------------------------------------------------------------------------------!
