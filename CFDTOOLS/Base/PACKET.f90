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
integer, parameter :: fct_buffer    = 1024     ! buffer for vector computation

! -- interface DECLARATIONS -----------------------------------------------------------

contains

!----------------------------------------------------------------------------------------
! legacy routine for computation of packet
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
! packet size computation with at least nblock=k*nthread and distribution of lacking part
!----------------------------------------------------------------------------------------
subroutine calc_buffersize(ntot, maxbuffer, nthread, nblock, nb1, buf1, buf2)
implicit none
  integer, intent(in)  :: ntot, maxbuffer, nthread ! total number of element and maximal buffer
  integer, intent(out) :: nblock                   ! number of packs/blocks
  integer, intent(out) :: nb1, buf1, buf2          ! computed buffer (for almost all blocks)

  nblock  = 1 + (ntot-1) / maxbuffer
  ! nblock must be multiple of nthread
  nblock  = ((nblock-1)/nthread + 1)*nthread
  buf2    = ntot/nblock        ! incomplete packet size
  buf1    = buf2+1
  nb1     = mod(ntot, nblock)
endsubroutine calc_buffersize

!----------------------------------------------------------------------------------------
! computation of starting and ending index of packet
!----------------------------------------------------------------------------------------
subroutine new_buf_index(ntot, maxbuffer, nblock, ista, iend, nthread)
implicit none
! -- dummy arguments --
  integer,          intent(in)  :: ntot, maxbuffer          ! total number of element and maximal buffer
  integer,          intent(out) :: nblock                   ! number of packs/blocks
  integer, pointer, intent(out) :: ista(:), iend(:)
  integer, optional             :: nthread
! -- private data --
  integer :: buffer, partbuffer, buf, ib, buf1, nb1, buf2
  
  if (present(nthread)) then
    call calc_buffersize(ntot, maxbuffer, nthread, nblock, nb1, buf1, buf2)
    allocate(ista(nblock))
    allocate(iend(nblock))
    do ib = 1, nb1    ! caution: nb1 can be zero
      ista(ib) = (ib-1)*buf1 + 1
      iend(ib) = ib*buf1
    enddo
    do ib = nb1+1, nblock
      ista(ib) = nb1*buf1 + (ib-nb1-1)*buf2 + 1
      iend(ib) = ista(ib)+buf2-1
    enddo
  else
    ! legacy computation : size of buffer is maximal and partbuffer has missing part
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
  endif

endsubroutine new_buf_index

!----------------------------------------------------------------------------------------
endmodule PACKET
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2011: creation, splitted from VARCOM
!------------------------------------------------------------------------------!
