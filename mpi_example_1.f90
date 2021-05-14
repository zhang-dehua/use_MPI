program jifen_mpi
implicit none
include 'mpif.h'
integer :: node,np,ierr,status(mpi_status_size)
logical :: Ionode
integer :: n,i
real ::a,b,x,h,jifen,pi

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,node,ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD,np,ierr)

Ionode=(node .eq. 0)

if (Ionode) then
    write(*,*) "Plese input n:"
    read(*,*) n
    do i=1,np-1
        call MPI_SEND(n,1,MPI_INTEGER,i,99,MPI_COMM_WORLD,ierr)
    end do
else
    call MPI_RECV(n,1,MPI_INTEGER,0,99,MPI_COMM_WORLD,status,ierr)
end if


write(*,*) "This is the ",node,"n=",n



call MPI_FINALIZE(ierr)

end program jifen_mpi