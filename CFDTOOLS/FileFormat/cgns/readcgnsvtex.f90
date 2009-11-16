!------------------------------------------------------------------------------!
! Procedure : readcgnsvtex                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture des sommets d'une zone
!
!------------------------------------------------------------------------------!
subroutine readcgnsvtex(unit, ib, iz, mesh)                 

use CGNS_STRUCT   ! Definition des structures CGNS
use IOCFD        ! Sorties standard TYPHON

implicit none

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone

! -- Sorties --
type(st_cgns_vtex)  :: mesh       ! sommets de la zone

! -- Variables internes --
integer, parameter   :: cgns_size = 8                                      
integer              :: ier        ! code erreur
real(4), allocatable :: vs(:,:,:)         ! tableau de valeurs intermediaires SINGLE 
real(8), allocatable :: vd(:,:,:)         ! tableau de valeurs intermediaires DOUBLE
integer              :: i, j, k
character(len=100)   :: str_w   ! nom fantome

! -- BODY --


write(str_w,*) " reading CGNS nodes :",mesh%ni,"x",mesh%nj,"x",mesh%nk
call cfd_print(trim(str_w))

select case(cgns_size)
case(4)

  allocate(vs(mesh%ni,mesh%nj,mesh%nk))
  ! Lecture de X
  call cg_coord_read_f(unit, ib, iz, 'CoordinateX', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%ni,mesh%nj,mesh%nk /), vs, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable read X coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%x = vs(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Y
  call cg_coord_read_f(unit, ib, iz, 'CoordinateY', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%ni,mesh%nj,mesh%nk /), vs, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable read Y coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%y = vs(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Z
  call cg_coord_read_f(unit, ib, iz, 'CoordinateZ', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%ni, mesh%nj,mesh%nk /), vs, ier)
  if (ier /= 0)   call cfd_error("(CGNS) unable read Z coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%z = vs(i,j,k)
      enddo
    enddo
  enddo

  deallocate(vs)

case(8)

  allocate(vd(mesh%ni,mesh%nj,mesh%nk))
  ! Lecture de X
  call cg_coord_read_f(unit, ib, iz, 'CoordinateX', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%ni,mesh%nj,mesh%nk /), vd, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable read X coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%x = vd(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Y
  call cg_coord_read_f(unit, ib, iz, 'CoordinateY', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%ni,mesh%nj,mesh%nk /), vd, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable read Y coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%y = vd(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Z
  call cg_coord_read_f(unit, ib, iz, 'CoordinateZ', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%ni, mesh%nj,mesh%nk /), vd, ier)
  if (ier /= 0)   call cfd_error("(CGNS) unable read Z coordinate")

  ! retranscription
  do k = 1, mesh%nk
    do j = 1, mesh%nj
      do i = 1, mesh%ni
        mesh%vertex(i,j,k)%z = vd(i,j,k)
      enddo
    enddo
  enddo

  deallocate(vd)

case default
  call cfd_error("(CGNS) internal error (defintion of CGNS real size)")
endselect

!------------------------------
endsubroutine readcgnsvtex
