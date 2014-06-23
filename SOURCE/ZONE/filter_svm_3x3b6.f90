!------------------------------------------------------------------------------!
!program to filter parasite oscillations with reduced basis case 3x3b6
!
! 				                  Authors : JIANG
!                                                 Created : Mars 2014
! Fonction
!   filter oscillations for five state variables
!
!------------------------------------------------------------------------------!
subroutine filter_svm_3x3b6(etatcons,nsv,ncv)

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
real,dimension(9)      :: out

! -- BODY --

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!filtre!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do isv=1,nsv


!!!for rho
	out(1) =0.62799153207185_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)+0.20534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(2) =0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.66666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(3) =-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.62799153207185_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)+0.20534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(4) =0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.66666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(5) =-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)+0.82136720504592_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(6) =-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)+0.66666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(7) =-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)+0.20534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)+0.62799153207185_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(8) =-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)+0.089316397477041_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)+0.66666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)+0.16666666666667_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)
	out(9) =0.20534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+2)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+5)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+6)-0.083333333333333_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+7)+0.45534180126148_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+8)+0.62799153207185_krp*etatcons%tabscal(1)%scal((isv-1)*ncv+9)

	etatcons%tabscal(1)%scal((isv-1)*ncv+1)=out(1)
	etatcons%tabscal(1)%scal((isv-1)*ncv+2)=out(2)
	etatcons%tabscal(1)%scal((isv-1)*ncv+3)=out(3)
	etatcons%tabscal(1)%scal((isv-1)*ncv+4)=out(4)
	etatcons%tabscal(1)%scal((isv-1)*ncv+5)=out(5)
	etatcons%tabscal(1)%scal((isv-1)*ncv+6)=out(6)
	etatcons%tabscal(1)%scal((isv-1)*ncv+7)=out(7)
	etatcons%tabscal(1)%scal((isv-1)*ncv+8)=out(8)
	etatcons%tabscal(1)%scal((isv-1)*ncv+9)=out(9)
!!!for rho vx
	out(1) =0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(2) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(3) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(4) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(5) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x+0.82136720504592_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(6) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(7) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(8) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x
	out(9) =0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%x=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%x=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%x=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%x=out(4)
	etatcons%tabvect(1)%vect((isv-1)*ncv+5)%x=out(5)
	etatcons%tabvect(1)%vect((isv-1)*ncv+6)%x=out(6)
	etatcons%tabvect(1)%vect((isv-1)*ncv+7)%x=out(7)
	etatcons%tabvect(1)%vect((isv-1)*ncv+8)%x=out(8)
	etatcons%tabvect(1)%vect((isv-1)*ncv+9)%x=out(9)
!!!for rho vy
	out(1) =0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(2) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(3) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(4) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(5) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y+0.82136720504592_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(6) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(7) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(8) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y
	out(9) =0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%y=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%y=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%y=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%y=out(4)
	etatcons%tabvect(1)%vect((isv-1)*ncv+5)%y=out(5)
	etatcons%tabvect(1)%vect((isv-1)*ncv+6)%y=out(6)
	etatcons%tabvect(1)%vect((isv-1)*ncv+7)%y=out(7)
	etatcons%tabvect(1)%vect((isv-1)*ncv+8)%y=out(8)
	etatcons%tabvect(1)%vect((isv-1)*ncv+9)%y=out(9)
!!!for rho vz
	out(1) =0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(2) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(3) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(4) =0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(5) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z+0.82136720504592_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(6) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(7) =-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z+0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(8) =-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z+0.089316397477041_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.04465819873852_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z+0.24401693585629_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z+0.66666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z+0.16666666666667_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z
	out(9) =0.20534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z-0.12200846792815_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z-0.33333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z-0.083333333333333_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z+0.45534180126148_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z+0.62799153207185_krp*etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z

	etatcons%tabvect(1)%vect((isv-1)*ncv+1)%z=out(1)
	etatcons%tabvect(1)%vect((isv-1)*ncv+2)%z=out(2)
	etatcons%tabvect(1)%vect((isv-1)*ncv+3)%z=out(3)
	etatcons%tabvect(1)%vect((isv-1)*ncv+4)%z=out(4)
	etatcons%tabvect(1)%vect((isv-1)*ncv+5)%z=out(5)
	etatcons%tabvect(1)%vect((isv-1)*ncv+6)%z=out(6)
	etatcons%tabvect(1)%vect((isv-1)*ncv+7)%z=out(7)
	etatcons%tabvect(1)%vect((isv-1)*ncv+8)%z=out(8)
	etatcons%tabvect(1)%vect((isv-1)*ncv+9)%z=out(9)
!!!for rho E 
	out(1) =0.62799153207185_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)+0.20534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(2) =0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.66666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(3) =-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.62799153207185_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)+0.20534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(4) =0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.66666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(5) =-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)+0.82136720504592_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(6) =-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)+0.66666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(7) =-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)+0.20534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)+0.62799153207185_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(8) =-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)+0.089316397477041_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.04465819873852_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)+0.24401693585629_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)+0.66666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)+0.16666666666667_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)
	out(9) =0.20534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+1)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+2)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+3)-0.12200846792815_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+4)-0.33333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+5)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+6)-0.083333333333333_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+7)+0.45534180126148_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+8)+0.62799153207185_krp*etatcons%tabscal(2)%scal((isv-1)*ncv+9)

	etatcons%tabscal(2)%scal((isv-1)*ncv+1)=out(1)
	etatcons%tabscal(2)%scal((isv-1)*ncv+2)=out(2)
	etatcons%tabscal(2)%scal((isv-1)*ncv+3)=out(3)
	etatcons%tabscal(2)%scal((isv-1)*ncv+4)=out(4)
	etatcons%tabscal(2)%scal((isv-1)*ncv+5)=out(5)
	etatcons%tabscal(2)%scal((isv-1)*ncv+6)=out(6)
	etatcons%tabscal(2)%scal((isv-1)*ncv+7)=out(7)
	etatcons%tabscal(2)%scal((isv-1)*ncv+8)=out(8)
	etatcons%tabscal(2)%scal((isv-1)*ncv+9)=out(9)

enddo
endsubroutine filter_svm_3x3b6

!------------------------------------------------------------------------------!
! Changes history
!
! Mars 2014 : created
!------------------------------------------------------------------------------!
