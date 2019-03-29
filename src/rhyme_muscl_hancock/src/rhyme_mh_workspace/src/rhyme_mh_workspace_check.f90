submodule ( rhyme_mh_workspace ) rhyme_mh_workspace_check_submodule
contains
  pure module subroutine rhyme_mh_workspace_check ( mhws, box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_box_t ), intent ( in ) :: box

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
    integer :: lb(3), ub(3), wslb(4), wsub(4), stat

    l = box%level
    b = box%number

    lb = lbound( box%hydro )
    ub = ubound( box%hydro )

    if ( allocated( mhws%levels(l)%boxes(b)%UL ) ) then

      wslb = lbound( mhws%levels(l)%boxes(b)%UL )
      wsub = ubound( mhws%levels(l)%boxes(b)%UL )

      if ( any( lb .ne. wslb(:3) ) .or. any( ub .ne. wsub(:3) ) ) then
        deallocate( mhws%levels(l)%boxes(b)%UL, stat=stat )
        deallocate( mhws%levels(l)%boxes(b)%UR, stat=stat )
        deallocate( mhws%levels(l)%boxes(b)%FR, stat=stat )

        allocate( mhws%levels(l)%boxes(b)%UL ( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
        allocate( mhws%levels(l)%boxes(b)%UR ( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
        allocate( mhws%levels(l)%boxes(b)%FR ( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
      end if
    else
      allocate( mhws%levels(l)%boxes(b)%UL ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
      allocate( mhws%levels(l)%boxes(b)%UR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
      allocate( mhws%levels(l)%boxes(b)%FR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 ))
    end if

  end subroutine mhws_check_memory_intensive


  pure module subroutine mhws_check_cpu_intensive ( mhws, box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_box_t ), intent ( in ) :: box

    integer :: l, b, d(3), lb(3), ub(3)

    l = box%level
    b = box%number
    d = box%dims

    if ( allocated( mhws%levels(l)%boxes(b)%U ) ) then
      lb = lbound( mhws%levels(l)%boxes(b)%U )
      ub = ubound( mhws%levels(l)%boxes(b)%U )

      if ( any( lb .ne. 1 ) .or. any( ub .ne. d ) ) then
        deallocate( mhws%levels(l)%boxes(b)%U )
        allocate( mhws%levels(l)%boxes(b)%U( 1:d(1), 1:d(2), 1:d(3) ) )
      end if
    else
      allocate( mhws%levels(l)%boxes(b)%U( 1:d(1), 1:d(2), 1:d(3) ) )
    end if
  end subroutine mhws_check_cpu_intensive

end submodule rhyme_mh_workspace_check_submodule
