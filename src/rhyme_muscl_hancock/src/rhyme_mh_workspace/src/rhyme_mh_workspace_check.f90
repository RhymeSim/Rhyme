submodule ( rhyme_mh_workspace ) rhyme_mh_workspace_check_submodule
contains
  pure module subroutine rhyme_mh_workspace_check ( mhws, box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_box_t ), intent ( in ) :: box

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#define RANGE_DIM_J
#define RANGE_DIM_K
#elif NDIM == 2
#define RANGE_J , lb(2):ub(2)
#define RANGE_K
#define RANGE_DIM_J , 1:d(2)
#define RANGE_DIM_K
#elif NDIM == 3
#define RANGE_J , lb(2):ub(2)
#define RANGE_K , lb(3):ub(3)
#define RANGE_DIM_J , 1:d(2)
#define RANGE_DIM_K , 1:d(3)
#endif

    if ( mhws%type .eq. mhwsid%memory_intensive ) then
      call mhws_check_memory_intensive( mhws, box )
    else if ( mhws%type .eq. mhwsid%cpu_intensive ) then
      call mhws_check_cpu_intensive( mhws, box )
    else
      call mhws_check_memory_intensive( mhws, box )
    end if

  end subroutine rhyme_mh_workspace_check


  pure module subroutine mhws_check_memory_intensive ( mhws, box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_box_t ), intent ( in ) :: box

    integer :: l, b
    integer :: lb( NDIM+1 ), ub( NDIM+1 ), wslb( NDIM+2 ), wsub( NDIM+2 ), stat

    l = box%level
    b = box%number

    lb = lbound( box%cells )
    ub = ubound( box%cells )

    if ( allocated( mhws%levels(l)%boxes(b)%ul ) ) then

      wslb = lbound( mhws%levels(l)%boxes(b)%ul )
      wsub = ubound( mhws%levels(l)%boxes(b)%ul )

      if ( any( lb .ne. wslb(:NDIM+1) ) .or. any( ub .ne. wsub(:NDIM+1) ) ) then
        deallocate( mhws%levels(l)%boxes(b)%ul, stat=stat )
        deallocate( mhws%levels(l)%boxes(b)%ur, stat=stat )
        deallocate( mhws%levels(l)%boxes(b)%fr, stat=stat )

        allocate( mhws%levels(l)%boxes(b)%ul( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
        allocate( mhws%levels(l)%boxes(b)%ur( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
        allocate( mhws%levels(l)%boxes(b)%fr( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
      end if
    else
        allocate( mhws%levels(l)%boxes(b)%ul( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
        allocate( mhws%levels(l)%boxes(b)%ur( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
        allocate( mhws%levels(l)%boxes(b)%fr( &
          lb(1):ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot, NDIM ))
    end if

  end subroutine mhws_check_memory_intensive


  pure module subroutine mhws_check_cpu_intensive ( mhws, box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_box_t ), intent ( in ) :: box

    integer :: l, b, d( NDIM ), lb( NDIM+1 ), ub( NDIM+1 )

    l = box%level
    b = box%number
    d = box%dims

    if ( allocated( mhws%levels(l)%boxes(b)%u ) ) then
      lb = lbound( mhws%levels(l)%boxes(b)%u )
      ub = ubound( mhws%levels(l)%boxes(b)%u )

      if ( any( lb( 1:NDIM ) .ne. 1 ) .or. any( ub( 1:NDIM ) .ne. d ) ) then
        deallocate( mhws%levels(l)%boxes(b)%u )
        allocate( mhws%levels(l)%boxes(b)%u( &
          1:d(1) RANGE_DIM_J RANGE_DIM_K, cid%rho:cid%e_tot ) )
      end if
    else
        allocate( mhws%levels(l)%boxes(b)%u( &
          1:d(1) RANGE_DIM_J RANGE_DIM_K, cid%rho:cid%e_tot ) )
    end if
  end subroutine mhws_check_cpu_intensive
end submodule rhyme_mh_workspace_check_submodule
