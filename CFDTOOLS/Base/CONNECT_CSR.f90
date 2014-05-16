!------------------------------------------------------------------------------!
! MODULE : CONNECT_CSR                              Authors: J. Gressier
!                                                   Created: October 2005
! Fonction
!   Compressed Row Storage format (definition & conversion)
!
!------------------------------------------------------------------------------!

module CONNECT_CSR

use CONNECTIVITY 
use LIBSORT         ! sorting procedure library
use IOCFD 

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure ST_CSR : Compressed Row Storage Definition
!------------------------------------------------------------------------------!
type st_csr
  integer(kip)          :: dim             ! matrix size
  integer(kip)          :: nval            ! number of no zero values
  integer(kip), pointer :: row_index(:)    ! index of new row in val(:)/col_index(:)  : size (dim+1)
  integer(kip), pointer :: col_index(:)    ! index of column  of val(:)               : size (nval)
endtype st_csr

!------------------------------------------------------------------------------!
! description of a N size sparse matrix
! - row_index(1:dim)  contains starting index of the i-th row in col_index(:) and val(:)
! - row_index(dim+1)  starts   at 1
! - row_index(dim+1)  contains (nval+1)
! - col_index(1:nval) contains column index of i-th value
!------------------------------------------------------------------------------!



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_csr, new_csr_fromfacecell
endinterface

interface delete
  module procedure delete_csr
endinterface

! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure CSR
!------------------------------------------------------------------------------!
subroutine new_csr(conn, dim, nval)
implicit none
type(st_csr) :: conn
integer(kip) :: dim, nval

  conn%dim  = dim
  conn%nval = nval
  allocate(conn%row_index(dim+1))
  allocate(conn%col_index(nval))

endsubroutine new_csr


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure CSR
!------------------------------------------------------------------------------!
subroutine delete_csr(conn)
implicit none
type(st_csr) :: conn

  conn%dim  = 0
  conn%nval = 0
  deallocate(conn%row_index)
  deallocate(conn%col_index)

endsubroutine delete_csr


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure CSR
!------------------------------------------------------------------------------!
subroutine new_csr_fromfacecell(csr, facecell, maxcell)
implicit none
type(st_csr)     :: csr
type(st_connect) :: facecell
integer(kip)     :: maxcell
! -- internal --
integer(kip)              :: dim, nval, nface
integer(kip), allocatable :: ncol(:)
integer(kip)              :: i, if, ic1, ic2

  if (maxcell == 0) maxcell = huge(maxcell)   ! use all faces
  dim = min(maxval(facecell%fils(1:facecell%nbnodes, 1:2)), maxcell)

  ! -- count number of col for each row & number of useful faces --

  allocate(ncol(dim))
  ncol(1:dim) = 0
  nface       = 0
  do if = 1, facecell%nbnodes
    ic1 = facecell%fils(if,1)
    ic2 = facecell%fils(if,2)
    if ((ic1 <= maxcell).and.(ic2 <= maxcell).and.(ic2 >= 1)) then
      nface     = nface     + 1
      ncol(ic1) = ncol(ic1) + 1
      ncol(ic2) = ncol(ic2) + 1
    endif
  enddo

  nval  = 2*nface
  call new(csr, dim, nval)

  ! -- fill row_index --

  csr%row_index(1) = 1
  do i = 2, dim+1
    csr%row_index(i) = csr%row_index(i-1) + ncol(i-1)
  enddo
  !print*, "nval",nval
  !print*, "ncol",ncol
  !print*, "row",csr%row_index
  if (csr%row_index(dim+1) /= nval+1) &
    call cfd_error("bad index while translating facecell to CSR")

  ! -- fill col_index --

  ncol(1:dim) = 0

  do if = 1, facecell%nbnodes
    ic1 = facecell%fils(if,1)
    ic2 = facecell%fils(if,2)
    if ((ic1 <= maxcell).and.(ic2 <= maxcell).and.(ic2 >= 1)) then
      csr%col_index(csr%row_index(ic1)+ncol(ic1)) = ic2
      csr%col_index(csr%row_index(ic2)+ncol(ic2)) = ic1
      ncol(ic1) = ncol(ic1) + 1
      ncol(ic2) = ncol(ic2) + 1
    endif
  enddo

  deallocate(ncol)

  ! -- sort each row --

  do i = 1, csr%dim
    call sort_insertion(csr%col_index(csr%row_index(i):csr%row_index(i+1)-1), &
                        csr%row_index(i)-csr%row_index(i+1))
  enddo

endsubroutine new_csr_fromfacecell



endmodule CONNECT_CSR

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005: Created
! May  2013: moved to CFDTOOLS
!------------------------------------------------------------------------------!



