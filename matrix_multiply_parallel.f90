program main
    use MPI
    implicit none
    integer :: my_id, dest_id, source_id, proc_num, ierr
    integer, dimension(MPI_STATUS_SIZE) :: status

    
    real(kind=8) :: x, y, S_local, S_global
    real(kind=8), allocatable, dimension(:,:) :: A, B, B_temp, C
    real(kind=8) :: time_start,time_end,time_total
    integer :: N, NP, i, j, k, step

    

    N = 2000
    CALL MPI_INIT(ierr)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,my_id,ierr)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD,proc_num,ierr)
    time_start = MPI_WTIME()
    NP=N/proc_num
    
    if (my_id == 0) then
        if( mod(N,proc_num)/=0 )then
            write(*,*) "Process 0: ERROR!"
            write(*,*) "The remainder of the division of N by roc_num should be zero."
            write(*,*) "The program is forced to abort!"
            CALL MPI_ABORT(MPI_COMM_WORLD,100,ierr)
        end if
    end if


    allocate(A(NP,N),B(N,NP),B_temp(N,NP),C(NP,N),STAT=ierr)

    ! Initialize A,B,C
    do j=1,N 
        do i=1,NP 
            x = (dble(my_id*NP+i)-1.0d0)/(dble(N)-1.0d0)
            y = (dble(j)-1.0d0)/(dble(N)-1.0d0)
            A(i,j) = exp(x)*sin(3.0d0*x)
        end do
    end do

    do j=1,Np
        do i=1,N 
            x = (dble(i)-1.0d0)/(dble(N)-1.0d0)
            y = (dble(my_id*NP+j)-1.0d0)/(dble(N)-1.0d0)
            B(i,j) = ( x+cos(4.0d0*x) )*(1.0d0+y)
        end do
    end do
    C = 0.0d0

    !
    do j=1,NP
        do i=1,NP
            do k=1,N
                C(i,my_id*NP+j)=C(i,my_id*NP+j)+A(i,k)*B(k,j)
            end do
        end do
    end do

    do step=1,proc_num-1
        if(my_id+step<=proc_num-1)then
            dest_id = my_id+step
        else
            dest_id = my_id+step-proc_num
        end if

        if(my_id-step>=0)then
            source_id = my_id-step
        else
            source_id = my_id-step+proc_num
        end if

        CALL MPI_SENDRECV(B,N*NP,MPI_DOUBLE_PRECISION,dest_id, step,&
        		        &B_temp,N*NP,MPI_DOUBLE_PRECISION,source_id,step,&
                        &MPI_COMM_WORLD,status,ierr)

        do j=1,NP
            do i=1,NP
                do k=1,N
                    C(i,source_id*NP+j)=C(i,source_id*NP+j)+A(i,k)*B_temp(k,j)
                end do
            end do
        end do

        CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    end do

    S_local = 0.0d0
    do j=1,N 
        do i=1,NP
            S_local = S_local+C(i,j)**2
        end do
    end do

    CALL MPI_REDUCE(S_local,S_global,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)

    S_global = S_global/N**2

    time_end = MPI_WTIME()
    time_total = time_end-time_start

    if (my_id == 0) then
            write(*,*) "===================================="
            write(*,*) "Process 0"
            write(*,*) "S_global=", S_global
            WRITE(*,*) "Total time is", time_total
            write(*,*) "===================================="
    end if

    deallocate(A,B,B_temp,C,STAT=ierr)
    CALL MPI_FINALIZE(ierr)
end program