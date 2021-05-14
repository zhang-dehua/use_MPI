PROGRAM sum_n
    USE MPI
    ! or the older form: INCLUDE ’mpif.h’
    IMPLICIT NONE
    INTEGER :: my_id, proc_num, ierr
    INTEGER :: sum_global, sum_local
    INTEGER :: n_start, n_global, i
    real(kind=8) :: time_start,time_end,time_total

    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)

    time_start = MPI_WTIME()
    n_global = 10000
    sum_local = 0
    n_start = my_id+1
    DO i=n_start,n_global,proc_num
        sum_local = sum_local+i
    END DO

    WRITE(*,*) " "
    WRITE(*,*) "==========================================="
    WRITE(*,10) my_id,proc_num
    10 FORMAT('Process ',I2,' of ',I2)
    WRITE(*,*) "The local caculation finished."
    WRITE(*,*) "n_start=", n_start, "n_end=", i-proc_num
    WRITE(*,*) "sum_local=", sum_local
    WRITE(*,*) "==========================================="

    CALL MPI_REDUCE(sum_local,sum_global,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    time_end = MPI_WTIME()
    time_total = time_end-time_start

    IF( my_id == 0 )THEN
        WRITE(*,*) " "
        WRITE(*,*) "==========================================="
        WRITE(*,*) "The global caculation finished."
        WRITE(*,*) "The process is ", my_id
        WRITE(*,*) "sum_local=", sum_global
        WRITE(*,*) "Total time is", time_total
        WRITE(*,*) "==========================================="
    END IF

    CALL MPI_FINALIZE(ierr)

END PROGRAM