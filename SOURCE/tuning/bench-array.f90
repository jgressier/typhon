program bench

integer, parameter :: dim = 10000000
real               :: itime, time, dt

real, dimension(dim) :: a, b
integer              :: i, ip, is, ie
integer              :: np, maxbuf, buf, modbuf, ibuf

a(1:dim) = 0.

do i = 1, dim
  b(i)    = sqrt(real(i))
enddo

! ---------------------------
call cpustart

do i = 1, dim
  a(i)    = a(i)+b(i)
enddo

call cpuend(dt)
print*,'direct loop :',dt

! ---------------------------
call cpustart

a(1:dim)    = a(1:dim) + b(1:dim)

call cpuend(dt)
print*,'matrix op.  :',dt

! ---------------------------

maxbuf = 1
do while (maxbuf <= dim)

   call calc_buffer(dim, maxbuf, np, buf, modbuf)
   call cpustart

   is   = 1
   ibuf = modbuf
   do ip = 1, np
      ie = is-1+ibuf
      a(is:ie)    = a(is:ie)+ b(is:ie)
      ibuf = buf
      is   = ie+1
   enddo
   call cpuend(dt)
   print*,'packet',maxbuf,':',np, buf, modbuf,dt
   maxbuf = maxbuf*2
enddo

! ---------------------------

call cpustart

do i = 1, dim, 4
  a(i)    = a(i)  + b(i)
  a(1+1)  = a(i+1)+ b(i+1)
  a(1+2)  = a(i+2)+ b(i+2)
  a(1+3)  = a(i+3)+ b(i+3)
enddo
call cpuend(dt)
print*,'inline ',4,':',dt


! ---------------------------

call cpustart

do i = 1, dim, 8
  a(i)    = a(i)  + b(i)
  a(1+1)  = a(i+1)+ b(i+1)
  a(1+2)  = a(i+2)+ b(i+2)
  a(1+3)  = a(i+3)+ b(i+3)
  a(1+4)  = a(i+4)+ b(i+4)
  a(1+5)  = a(i+5)+ b(i+5)
  a(1+6)  = a(i+6)+ b(i+6)
  a(1+7)  = a(i+7)+ b(i+7)
enddo
call cpuend(dt)
print*,'inline ',8,':',dt



contains

subroutine cpustart()
  call cpu_time(itime)
end subroutine cpustart

subroutine cpuend(deltat)
real :: deltat
  call cpu_time(time)
  deltat = time-itime 
endsubroutine cpuend

endprogram


subroutine calc_buffer(ntot, maxbuffer, nblock, resbuffer, partbuffer)

  integer, intent(in)  :: ntot, maxbuffer          ! total number of element and maximal buffer
  integer, intent(out) :: nblock                   ! number of packs/blocks
  integer, intent(out) :: resbuffer                ! computed buffer (for almost all blocks)
  integer, intent(out) :: partbuffer               ! small block (residue of distribution)
  
  nblock     = 1 + (ntot-1) / maxbuffer
  resbuffer  = 1 + (ntot-1) / nblock        
  partbuffer = 1 + mod(ntot-1, resbuffer)

endsubroutine

