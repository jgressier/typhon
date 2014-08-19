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
use MESHBASE

implicit none

! -- INPUTS --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone

! -- OUTPUTS --
type(st_mesh)  :: mesh       ! MESH vertices

! -- Internal variables --
integer, parameter   :: cgns_size = 8
integer              :: ier               ! code erreur
real(4), allocatable :: vs(:,:,:)         ! intermediate SINGLE array
real(8), allocatable :: vd(:,:,:)         ! intermediate DOUBLE array
integer              :: i, j, k
character(len=100)   :: str_w   ! nom fantome

! -- BODY --

call cfd_print("- reading CGNS nodes: "//trim(strof(mesh%idim))//"x"//trim(strof(mesh%jdim))//"x"//trim(strof(mesh%kdim)))

select case(cgns_size)
case(4)

  allocate(vs(mesh%idim,mesh%jdim,mesh%kdim))
  ! Lecture de X
  call cg_coord_read_f(unit, ib, iz, 'CoordinateX', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%idim,mesh%jdim,mesh%kdim /), vs, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable to read X coordinate")

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%x = vs(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Y
  call cg_coord_read_f(unit, ib, iz, 'CoordinateY', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%idim,mesh%jdim,mesh%kdim /), vs, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable to read Y coordinate")

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%y = vs(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Z
  call cg_coord_read_f(unit, ib, iz, 'CoordinateZ', RealSingle, (/ 1, 1, 1/), &
       (/ mesh%idim, mesh%jdim,mesh%kdim /), vs, ier)
  if (ier /= 0) then
    vs(:,:,:) = 0._krp
  endif

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%z = vs(i,j,k)
      enddo
    enddo
  enddo

  deallocate(vs)

case(8)

  allocate(vd(mesh%idim,mesh%jdim,mesh%kdim))
  ! Lecture de X
  call cg_coord_read_f(unit, ib, iz, 'CoordinateX', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%idim,mesh%jdim,mesh%kdim /), vd, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable to read X coordinate")

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%x = vd(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Y
  call cg_coord_read_f(unit, ib, iz, 'CoordinateY', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%idim,mesh%jdim,mesh%kdim /), vd, ier)
  if (ier /= 0) call cfd_error("(CGNS) unable to read Y coordinate")

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%y = vd(i,j,k)
      enddo
    enddo
  enddo

  ! Lecture de Z
  call cg_coord_read_f(unit, ib, iz, 'CoordinateZ', RealDouble, (/ 1, 1, 1/), &
       (/ mesh%idim, mesh%jdim,mesh%kdim /), vd, ier)
  if (ier /= 0) then
    vd(:,:,:) = 0._krp
  endif

  ! retranscription
  do k = 1, mesh%kdim
    do j = 1, mesh%jdim
      do i = 1, mesh%idim
        mesh%vertex(i,j,k)%z = vd(i,j,k)
      enddo
    enddo
  enddo

  deallocate(vd)

case default
  call cfd_error("(CGNS) internal error (definition of CGNS real size)")
endselect

!------------------------------
endsubroutine readcgnsvtex
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! dec  2010 : fill directly meshbase structure
!------------------------------------------------------------------------------!
