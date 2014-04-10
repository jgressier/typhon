!------------------------------------------------------------------------------!
! MODULE : LIBRLE                                  Authors: J. Gressier
!                                                   Created: October 2005
! Fonction
!   Run Length Encoding
!
!------------------------------------------------------------------------------!
module LIBRLE

use MESHPREC        ! accuracy definition
use CONNECTIVITY 
use IOCFD 

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! encode array into RLE description with connect structure
!------------------------------------------------------------------------------!
subroutine rle_encode(n, array, rlearray)
implicit none
! -- INPUTS --
integer(kip), intent(in) :: n
integer(kip), intent(in) :: array(:)
! -- OUTPUTS --
type(st_connect), intent(out) :: rlearray
! -- private data --
integer(kip), parameter :: buf = 1000
integer(kip)            :: val
integer(kip)            :: ia, irle, lrle

! -- BODY --

call new_connect(rlearray, buf, 2)
ia   = 1
irle = 0
do while (ia <= n)
  irle = irle + 1
  if (irle > rlearray%nbnodes) call realloc_connect(rlearray, irle-1+buf, 2)
  val  = array(ia)
  lrle = 1
  do while ((array(ia+1)==val).and.(ia < n))
    ia   = ia   + 1
    lrle = lrle + 1
  enddo
  rlearray%fils(irle, 1) = val
  rlearray%fils(irle, 2) = lrle
  ia = ia +1
enddo
rlearray%nbnodes = irle

endsubroutine rle_encode

!------------------------------------------------------------------------------!
! decode RLE to array
!------------------------------------------------------------------------------!
subroutine rle_decode(rlearray, array)
implicit none
! -- INPUTS --
type(st_connect), intent(in) :: rlearray
! -- OUTPUTS --
integer(kip), intent(out) :: array(:)
! -- private data --
integer(kip), parameter :: buf = 1000
integer(kip)            :: val
integer(kip)            :: ia, irle, lrle

! -- BODY --

ia = 0
do irle = 1, rlearray%nbnodes
  array(ia+1:ia+rlearray%fils(irle, 2)) = rlearray%fils(irle, 1)
  ia = ia + rlearray%fils(irle, 2)
enddo

endsubroutine rle_decode

!------------------------------------------------------------------------------!
! encode array into RLE description with connect structure
!------------------------------------------------------------------------------!
integer(kip) function rle_length(rlearray)
implicit none
! -- INPUTS --
type(st_connect), intent(in) :: rlearray
! -- private data --
! -- BODY --
  rle_length = sum(rlearray%fils(1:rlearray%nbnodes, 2))
endfunction


endmodule LIBRLE
!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2014: Created
!------------------------------------------------------------------------------!



