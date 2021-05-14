program main
    use mpi
    integer :: my_id, proc_num, ierr
    integer, dimension(MPI_STATUS_SIZE) :: status
    integer :: m_num, n_num
    real(kind=8), allocatable, dimension(:,:) :: A
    real(kind=8), allocatable, dimension(:) :: B, C, buffer
    real(kind=8) :: C_element    
    integer :: manager, worker_num
    integer :: i, j, sent_num, sender, row

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, proc_num, ierr)

    manager = 0
    m_num = 100
    n_num = 100
    worker_num = proc_num-1

    if(worker_num>m_num)then
        if (my_id == manager) then
            write(*,*) "Process manager : ERROR!"
            write(*,*) "The worker process number is larger than colums number of vector C!"
            write(*,*) "The program is forced to abort!"
            CALL MPI_ABORT(MPI_COMM_WORLD,100,ierr)
        end if
    end if

    allocate(B(n_num),buffer(n_num), STAT=ierr)

    if (my_id == manager) then
        allocate(A(m_num,n_num),C(m_num), STAT=ierr)
        do j = 1,n_num
            B(j) = 1
            do i = 1,m_num
                A(i,j) = i
            enddo
        enddo
        ! send b to each worker process
        call MPI_BCAST(b, n_num, MPI_DOUBLE_PRECISION, manager, MPI_COMM_WORLD, ierr)
        ! send a row to each worker process; tag with row number
        sent_num = 0
        do i = 1, worker_num
            buffer(:) = a(i,:)
            call MPI_SEND(buffer, n_num, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, ierr)
            sent_num = sent_num+1
        enddo
        do i = 1,m_num
            call MPI_RECV(C_element, 1, MPI_DOUBLE_PRECISION, &
            MPI_ANY_SOURCE, MPI_ANY_TAG, &
            MPI_COMM_WORLD, status, ierr)
            sender = status(MPI_SOURCE)
            row = status(MPI_TAG) ! row is tag value
            c(row) = C_element
            if (sent_num < m_num) then ! send another row
                do j = 1,n_num
                    buffer(j) = a(sent_num+1,j)
                enddo
                call MPI_SEND(buffer, n_num, MPI_DOUBLE_PRECISION, &
                sender, sent_num+1, MPI_COMM_WORLD, ierr)
                sent_num = sent_num+1
            else ! Tell sender that there is no more work
                call MPI_SEND(MPI_BOTTOM, 0, MPI_DOUBLE_PRECISION, &
            sender, 0, MPI_COMM_WORLD, ierr)
            endif
        enddo
        
    else
        ! workers receive b, then compute dot products until
        ! done message received
        call MPI_BCAST(b, n_num, MPI_DOUBLE_PRECISION, manager, &
                        MPI_COMM_WORLD, ierr)

        do
            call MPI_RECV(buffer, n_num, MPI_DOUBLE_PRECISION, &
            manager, MPI_ANY_TAG, MPI_COMM_WORLD, &
            status, ierr)
            if (status(MPI_TAG) == 0) exit
            row = status(MPI_TAG)
            C_element = 0.0
            do i = 1,n_num
                C_element = C_element+buffer(i)*b(i)
            enddo
            call MPI_SEND(C_element, 1, MPI_DOUBLE_PRECISION, manager, &
            row, MPI_COMM_WORLD, ierr)
        enddo
    endif

    if (my_id .eq. manager) then
        write(*,*) "============================================"
        write(*,*) "This is the manager process ", my_id
        do i=1,m_num
        write(*,"('C[',I3,']=',F10.1)") i, c(i)
        end do
        write(*,*) "============================================"
        deallocate(A, C, STAT=ierr)
    end if

    deallocate(B, buffer, STAT=ierr)
    
    call MPI_FINALIZE(ierr)
end