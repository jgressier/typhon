!------------------------------------------------------------------------------!
! MODULE : VTKFMT
!
!------------------------------------------------------------------------------!
module VTKFMT

use MESHPREC         ! Precision configuration

implicit none

! -- Global Variables -------------------------------------------


! -- Parameters --

integer(kip), parameter :: vtkbufsize = 10000

integer(kpp), parameter :: vtk_asc = 10
integer(kpp), parameter :: vtk_bin = 20

character,    parameter :: newline_char  = char(10)
character,    parameter :: carriage_char = char(13)

!------------------------------------------------------------------------------!
! definition of VTK output
!------------------------------------------------------------------------------!
type st_defvtk
  integer      :: iunit
  integer(kpp) :: type
endtype st_defvtk

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! open & write VTK header definition (ASCII)
!------------------------------------------------------------------------------!
subroutine vtkasc_openwrite(iunit, filename, defvtk)
implicit none
! -- INPUTS --
integer              :: iunit
character(len=*)     :: filename
! -- OUTPUTS --
type(st_defvtk)      :: defvtk
! -- private data --
integer              :: info
! -- BODY --

defvtk%iunit = iunit
defvtk%type  = vtk_asc

open(unit=iunit, file=trim(filename), form='formatted', iostat = info)
write(iunit,'(A)') '# vtk DataFile Version 2.0'
write(iunit,'(A)') 'TYPHON'
write(iunit,'(A)') 'ASCII'

endsubroutine vtkasc_openwrite

!------------------------------------------------------------------------------!
! open & write VTK header definition (BINARY)
!------------------------------------------------------------------------------!
subroutine vtkbin_openwrite(iunit, filename, defvtk)
implicit none
! -- INPUTS --
integer              :: iunit
character(len=*)     :: filename
! -- OUTPUTS --
type(st_defvtk)      :: defvtk
! -- private data --
integer              :: info
! -- BODY --

defvtk%iunit = iunit
defvtk%type  = vtk_bin

open(unit=iunit, file=trim(filename), convert='BIG_ENDIAN', form='unformatted', access='stream', iostat = info)
call writestr(iunit, '# vtk DataFile Version 2.0')
call writestr(iunit, 'TYPHON')
call writestr(iunit, 'BINARY')

endsubroutine vtkbin_openwrite

!------------------------------------------------------------------------------!
! Procedure : writestr
!------------------------------------------------------------------------------!
subroutine writestr(unit, str)
implicit none
integer,          intent(in) :: unit
character(len=*), intent(in) :: str

  write(unit) str//newline_char

endsubroutine writestr


!------------------------------------------------------------------------------!
! Procedure : writereturn
!------------------------------------------------------------------------------!
subroutine writereturn(unit)
implicit none
integer,          intent(in) :: unit

  write(unit) newline_char

endsubroutine writereturn




endmodule VTKFMT
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created, from TYPHON/SOURCE routines
!------------------------------------------------------------------------------!
