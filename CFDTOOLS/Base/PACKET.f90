!----------------------------------------------------------------------------------------
! MODULE : PACKET
! 
! Function
!   Variables globales du code TYPHON
!
!----------------------------------------------------------------------------------------
module PACKET

! -- PUBLIC Variables -------------------------------------------

integer, parameter :: cell_buffer   = 256      ! buffer for vector computation
integer, parameter :: face_buffer   = 256      ! buffer for vector computation
integer, parameter :: taille_buffer = 256      ! buffer for vector computation


! -- interface DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
subroutine calc_buffer(ntot, maxbuffer, nblock, buffer, partbuffer)
implicit none
  integer, intent(in)  :: ntot, maxbuffer          ! total number of element and maximal buffer
  integer, intent(out) :: nblock                   ! number of packs/blocks
  integer, intent(out) :: buffer                ! computed buffer (for almost all blocks)
  integer, intent(out) :: partbuffer               ! small block (residue of distribution)
  
  nblock     = 1 + (ntot-1) / maxbuffer
  buffer     = 1 + (ntot-1) / nblock        
  partbuffer = 1 + mod(ntot-1, buffer)
endsubroutine calc_buffer

!----------------------------------------------------------------------------------------
subroutine new_buf_index(ntot, maxbuffer, nblock, ista, iend)
implicit none
! -- dummy arguments --
  integer,          intent(in)  :: ntot, maxbuffer          ! total number of element and maximal buffer
  integer,          intent(out) :: nblock                   ! number of packs/blocks
  integer, pointer, intent(out) :: ista(:), iend(:)
! -- private data --
  integer :: buffer, partbuffer, buf, ib
  
  call calc_buffer(ntot, maxbuffer, nblock, buffer, partbuffer)
  buf  = partbuffer
  allocate(ista(nblock))
  allocate(iend(nblock))
  ista(1) = 1
  do ib = 1, nblock-1
    iend(ib)   = ista(ib)+buf-1
    ista(ib+1) = ista(ib)+buf
    buf  = buffer         ! tous les nblocks suivants sont de taille dimbuf
  enddo
  iend(nblock)   = ista(nblock)+buf-1

endsubroutine new_buf_index

!----------------------------------------------------------------------------------------
endmodule PACKET
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2011: creation, splitted from VARCOM
!------------------------------------------------------------------------------!
