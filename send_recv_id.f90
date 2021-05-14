PROGRAM send_source_id
    USE MPI
    ! or the older form: INCLUDE ’mpif.h’
    IMPLICIT NONE
    INTEGER :: my_id, proc_num, tag, ierr
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    REAL(KIND=8) :: time_start,time_end,time_total
    INTEGER :: dest, source
    INTEGER :: my_data, left_data, right_data

    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)

    time_start = MPI_WTIME()
    my_data = my_id+1000

    ! send data to left process
    tag = 10
    IF (my_id-1<0)THEN
        dest = my_id-1+proc_num
    ELSE
        dest = my_id-1
    END IF

    IF (my_id+1>=proc_num)THEN
        source = my_id+1-proc_num
    ELSE
        source = my_id+1
    END IF
    ! CALL MPI_SEND(my_data,1,MPI_INTEGER,dest,tag,MPI_COMM_WORLD,ierr)
    ! CALL MPI_RECV(right_data,1,MPI_INTEGER,source,tag,MPI_COMM_WORLD,status,ierr)
    ! CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_SENDRECV(my_data, 1, MPI_INTEGER, dest, tag,&
        		    &right_data, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD,&
        		    &status, ierr)

    ! send data to right process
    tag = 20
    IF (my_id-1<0)THEN
        source = my_id-1+proc_num
    ELSE
        source = my_id-1
    END IF

    IF (my_id+1>=proc_num)THEN
        dest = my_id+1-proc_num
    ELSE
        dest = my_id+1
    END IF

    ! CALL MPI_SEND(my_data,1,MPI_INTEGER,dest,tag,MPI_COMM_WORLD,ierr)
    ! CALL MPI_RECV(left_data,1,MPI_INTEGER,source,tag,MPI_COMM_WORLD,status,ierr)
    ! CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_SENDRECV(my_data, 1, MPI_INTEGER, dest, tag,&
    &left_data, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD,&
    &status, ierr)

    ! Write resualts
    WRITE(*,*) " "
    WRITE(*,*) "==========================================="
    WRITE(*,10) my_id,proc_num
    10 FORMAT('Process ',I2,' of ',I2)
    WRITE(*,*) "my_data = ", my_data
    WRITE(*,*) "left_data=", left_data, "right_data=", right_data
    WRITE(*,*) "==========================================="

    time_end = MPI_WTIME()
    time_total = time_end-time_start
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

    IF( my_id == 0 )THEN
        WRITE(*,*) "   "
        WRITE(*,*) "==========================================="
        WRITE(*,*) "Process is ", my_id
        WRITE(*,*) "The massage passing is finished."
        WRITE(*,*) "Total time is ", time_total, "seconds"
        WRITE(*,*) "==========================================="
    END IF

    CALL MPI_FINALIZE(ierr)

END PROGRAM