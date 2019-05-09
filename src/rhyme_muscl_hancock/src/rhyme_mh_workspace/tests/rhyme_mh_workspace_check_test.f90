logical function rhyme_mh_workspace_check_test () result ( failed )
  use rhyme_mh_workspace
  use rhyme_samr_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mhws_tester

  type ( mh_workspace_t ) :: ws
  type ( samr_t ) :: samr
  type ( log_t ) :: logger
  integer :: l, b, lb(3), ub(3), lbws(4), ubws(4), dims(3)

  mhws_tester = .describe. "mhws_check"

  logger = log_factory%generate()
  samr = samr_factory%generate()

  call rhyme_mh_workspace_init( ws, samr, logger )

  do l = 0, samr%nlevels
    do b = 1, samr%levels(l)%nboxes
      ! Check memory_intensive
      ws%type = mhwsid%memory_intensive
      call rhyme_mh_workspace_check( ws, samr%levels(l)%boxes(b) )

      lb = lbound ( samr%levels(l)%boxes(b)%hydro )
      ub = ubound ( samr%levels(l)%boxes(b)%hydro )

      dims = ub - lb + 1

      lbws = lbound ( ws%levels(l)%boxes(b)%UL )
      ubws = ubound ( ws%levels(l)%boxes(b)%UL )

      call mhws_tester%expect( allocated( ws%levels(l)%boxes(b)%UL ) .toBe. .true. )
      call mhws_tester%expect( allocated( ws%levels(l)%boxes(b)%UR ) .toBe. .true. )
      call mhws_tester%expect( allocated( ws%levels(l)%boxes(b)%FR ) .toBe. .true. )
      call mhws_tester%expect( size( ws%levels(l)%boxes(b)%UL ) .toBe. product( dims ) * 3 )
      call mhws_tester%expect( size( ws%levels(l)%boxes(b)%UR ) .toBe. product( dims ) * 3 )
      call mhws_tester%expect( size( ws%levels(l)%boxes(b)%FR ) .toBe. product( dims ) * 3 )
      call mhws_tester%expect( lb .toBe. lbws(:3) )
      call mhws_tester%expect( ub .toBe. ubws(:3) )

      ! Check cpu_intensive
      ws%type = mhwsid%cpu_intensive
      call rhyme_mh_workspace_check( ws, samr%levels(l)%boxes(b) )

      dims = samr%levels(l)%boxes(b)%dims
      lb = lbound( ws%levels(l)%boxes(b)%U )
      ub = ubound( ws%levels(l)%boxes(b)%U )

      call mhws_tester%expect( allocated( ws%levels(l)%boxes(b)%U ) .toBe. .true. )
      call mhws_tester%expect( size( ws%levels(l)%boxes(b)%U ) .toBe. product( dims ) )
      call mhws_tester%expect( lb .toBe. 1 )
      call mhws_tester%expect( ub .toBe. dims )
    end do
  end do

  failed = mhws_tester%failed()
end function rhyme_mh_workspace_check_test
