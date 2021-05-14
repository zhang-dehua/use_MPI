program main
    use mpi
    implicit none
    real(kind=8), parameter :: PI25DT = 3.141592653589793238462643d0
    real(kind=8) :: my_pi, pi, h, sum, x, f, a
    REAL(KIND=8) :: time_start,time_end,time_total
    integer ::  n, my_id, proc_num, i, ierr


    ! function to integrate
    f(a) = 4.d0 / (1.d0 + a*a)

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, proc_num, ierr)

    
    do
        
        if (my_id == 0) then
            write(*,*), "Enter the number of intervals: (0 quits) "
            read(*,*) n
        endif
        
        time_start = MPI_WTIME()
        ! broadcast n
        call MPI_BCAST(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

        ! check for quit signal
        if (n <= 0) then
            exit
        end if

        ! calculate the interval size
        h = 1.0d0/n
        sum = 0.0d0
        do i = my_id+1, n, proc_num
            x = h * (dble(i) - 0.5d0)
            sum = sum + f(x)
        enddo
        my_pi = h * sum
        ! collect all the partial sums
        call MPI_REDUCE(my_pi, pi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        time_end = MPI_WTIME()
        time_total = time_end-time_start
        ! node 0 prints the C_elementwer.
        if (my_id .eq. 0) then
            write(*,*) "   "
            write(*,*) "==========================================="
            write(*,*), "pi is ’, pi, ’ Error is", abs(pi - PI25DT)
            WRITE(*,*) "Total time is", time_total
            write(*,*) "==========================================="
        endif
    end do

    call MPI_FINALIZE(ierr)
end