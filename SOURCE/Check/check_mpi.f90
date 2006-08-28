program check

implicit none

include 'mpif.h'

integer :: my_id, ncpu
integer :: ierr, i
integer :: status(MPI_STATUS_SIZE)
real(4) :: r4, res4
real(8) :: r8, res8

call MPI_Init(ierr)
call MPI_Comm_rank(MPI_COMM_WORLD, my_id, ierr)
call MPI_Comm_size(MPI_COMM_WORLD, ncpu, ierr)
print*,"initialization MPI exchanges:",my_id,"over",ncpu,"cpu(s)"

if (my_id == 0) then
  do i = 2, ncpu
    call MPI_RECV(res4, 1, MPI_REAL, i-1, 1, MPI_COMM_WORLD, status, ierr)
    print*,"[",my_id,"]","recv REAL from cpu",i-1,":",res4
  enddo
else
  r4 = 10.*my_id
  print*,"[",my_id,"]","send REAL   to cpu",0,":",r4
  call MPI_SEND(r4, 1, MPI_REAL, 0, 1, MPI_COMM_WORLD,  ierr)
endif

if (my_id == 0) then
  do i = 2, ncpu
    call MPI_RECV(res8, 1, MPI_DOUBLE_PRECISION, i-1, 1, MPI_COMM_WORLD, status, ierr)
    print*,"[",my_id,"]","recv DOUBLE_PRECISION from cpu",i-1,":",res8
  enddo
else
  r8 = 10.*my_id
  print*,"[",my_id,"]","send DOUBLE_PRECISION   to cpu",0,":",r8
  call MPI_SEND(r8, 1, MPI_DOUBLE_PRECISION, 0, 1, MPI_COMM_WORLD,  ierr)
endif

print*,"[",my_id,"]","finalize MPI exchanges"
call MPI_Finalize(ierr)

endprogram
