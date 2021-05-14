program main
    use MPI
    implicit none
    integer :: my_id, dest_id, source_id, proc_num, ierr
    integer, dimension(MPI_STATUS_SIZE) :: status
    real(kind=8) :: time_start,time_end,time_total
    integer :: NX, NXP, NY, NYP, gc_num, nodex, nodey
    real(kind=8), allocatable, dimension(:) :: x, y
    real(kind=8), allocatable, dimension(:,:) :: T
    real(kind=8) :: x_min, x_max, y_min, y_max
    real(kind=8) :: dx, dy, x_start, x_end, y_start, y_end
    integer :: i, j

    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)
    time_start = MPI_WTIME()

    nodex=4
    nodey=5
    NX=200
    NY=300
    gc_num=2
    x_min=0.0d0
    x_max=2.0d0
    y_min=0.0d0
    y_max=3.0d0
    NXP=NX/nodex
    NYP=NY/nodey

    if (my_id == 0) then
        if( nodex*nodey/=proc_num )then
            write(*,*) "Process 0: ERROR!"
            write(*,*) "nodex*nodey/=proc_num!"
            write(*,*) "The program is forced to abort!"
            CALL MPI_ABORT(MPI_COMM_WORLD,100,ierr)
        end if
    end if

    allocate(x(1-gc_num:NXP+gc_num),y(1-gc_num:NYP+gc_num),T(1-gc_num:NXP+gc_num,1-gc_num:NYP+gc_num),STAT=ierr)

    ! set mesh
    dx=(x_max-x_min)/dble(NX)
    dy=(y_max-y_min)/dble(NY)
    x_start = x_min + (dble(my_id*NXP)+0.5d0)*dx
    y_start = y_min + (dble(my_id*NYP)+0.5d0)*dy

    do i=1,NXP
        x()
    end do
    do j=1,NYP
    end do


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