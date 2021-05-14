PROGRAM hello_world
    USE MPI
    ! or the older form: INCLUDE ’mpif.h’
    IMPLICIT NONE
    INTEGER :: my_id, proc_num, ierr, name_len
    CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME) :: processor_name
    
    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)
    CALL MPI_GET_PROCESSOR_NAME(processor_name, name_len, ierr)
    WRITE(*,10) my_id,proc_num,processor_name
    10 FORMAT('Hello World! Process ',I2,' of ',I2,' on ', 20A)
    CALL MPI_FINALIZE(ierr)

END PROGRAM