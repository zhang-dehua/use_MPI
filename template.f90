program main
    use MPI
    implicit none
    integer :: my_id, dest_id, source_id, proc_num, ierr
    integer, dimension(MPI_STATUS_SIZE) :: status
    real(kind=8) :: time_start,time_end,time_total


    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)
    time_start = MPI_WTIME()



    if (my_id == 0) then
        if( 1=proc_num )then
            write(*,*) "Process 0: ERROR!"
            write(*,*) "nodex*nodey/=proc_num!"
            write(*,*) "The program is forced to abort!"
            CALL MPI_ABORT(MPI_COMM_WORLD,100,ierr)
        end if
    end if



    time_end = MPI_WTIME()
    time_total = time_end-time_start
    if (my_id == 0) then
        write(*,*) "===================================="
        write(*,*) "Process 0"
        WRITE(*,*) "Total time is", time_total
        write(*,*) "===================================="
    end if

    CALL MPI_FINALIZE(ierr)

end program