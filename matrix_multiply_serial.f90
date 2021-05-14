program main
    use MPI
    implicit none

    integer :: N
    integer :: i, j, k, ierr
    real(kind=8) :: x, y, S
    real(kind=8), allocatable, dimension(:,:) :: A, B, C
    real(kind=8) :: time_start,time_end,time_total

    time_start = MPI_WTIME()

    N = 1000
    allocate(A(N,N),B(N,N),C(N,N),STAT=ierr)

    do j=1,N 
        do i=1,N 
            x = (dble(i)-1.0d0)/(dble(N)-1.0d0)
            y = (dble(j)-1.0d0)/(dble(N)-1.0d0)
            A(i,j) = exp(x)*sin(3.0d0*x)
            B(i,j) = ( x+cos(4.0d0*x) )*(1.0d0+y)
        end do
    end do

    C = 0.0d0
    do j=1,N 
        do i=1,N 
            do k=1,N
                C(i,j)=C(i,j)+A(i,k)*B(k,j)
            end do
        end do
    end do

    S = 0.0d0
    do j=1,N 
        do i=1,N 
            S = S+C(i,j)**2
        end do
    end do
    S = S/N**2

    time_end = MPI_WTIME()
    time_total = time_end-time_start

    write(*,*) "   "
    write(*,*) "===================================="
    write(*,*) "S=", S
    WRITE(*,*) "Total time is", time_total
    write(*,*) "===================================="

end program