!------------------------------------------------------------------------------!
!program to filter parasite oscillations with reduced basis case 2x2b3
!
! 				                  Authors : JIANG
!                                                 Created : Mars 2014
! Fonction
!   filter oscillations for five state variables
!
!------------------------------------------------------------------------------!
subroutine filter_svm_2x2b3(etatcons,nsv,ncv)

use DEFZONE
use TYPHMAKE
use VARCOM
use COMMTAG

implicit none


! -- INPUTS --
type(st_genericfield) :: etatcons
!type(mnu_solver)  :: defsolver
integer           :: nsv,ncv
! -- INPUTS/OUTPUTS --

! -- Internal variables --
integer   :: isv
real,dimension(4)      :: out

! -- BODY --

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!filtre!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do isv=1,nsv

!!!for rho
	out(1) =0.75_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)-0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)
	out(2)=0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.75_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)
	out(3)=0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)-0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.75_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)
	out(4)=-0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.25_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.75_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)

	etatcons%tabscal(1)%scal((isv-1)*ncv+1)=out(1)
	etatcons%tabscal(1)%scal((isv-1)*ncv+2)=out(2)
	etatcons%tabscal(1)%scal((isv-1)*ncv+3)=out(3)
	etatcons%tabscal(1)%scal((isv-1)*ncv+4)=out(4)
!!!for rho vx 
	out(1) =0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x
	out(2)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x
	out(3)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x
	out(4)=-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x=out(4)
!!!for rho vy
	out(1) =0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y
	out(2)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y
	out(3)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y
	out(4)=-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y=out(4) 
!!!for rho vz 
	out(1) =0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z
	out(2)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z
	out(3)=0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z
	out(4)=-0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.25_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.75_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z=out(4)
!!!for rho E 
	out(1) =0.75_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)-0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)
	out(2)=0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.75_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)
	out(3)=0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)-0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.75_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)
	out(4)=-0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.25_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.75_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)

	etatcons%tabscal(2)%scal((isv-1)*ncv+1)=out(1)
	etatcons%tabscal(2)%scal((isv-1)*ncv+2)=out(2)
	etatcons%tabscal(2)%scal((isv-1)*ncv+3)=out(3)
	etatcons%tabscal(2)%scal((isv-1)*ncv+4)=out(4)
enddo

endsubroutine filter_svm_2x2b3

!------------------------------------------------------------------------------!
! Changes history
!
! Mars 2014 : created
!------------------------------------------------------------------------------!
