submodule ( rhyme_assertion ) rhyme_assertion_passed_submodule
contains
  logical module function rhyme_assertion_passed ( this ) result ( passed )
    use rhyme_logger

    implicit none

    class ( assertion_t ), intent ( in ) :: this

    type ( test_t ), pointer :: test
    type ( logger_t ) :: logger

    integer :: nft, npt
    character ( len=128 ) :: msg
    character ( len=20 ) :: exp, got

    call logger%begin_section( this%desc )

    passed = .true.
    nft = 0
    npt = 0

    test => this%tests

    if ( .not. associated( test ) ) then
      call logger%warn( 'Nothing to test.' )
      return
    end if

    do while ( associated( test ) )
      call logger%begin_section( test%msg )

      if ( test%is_passed ) then
        npt = npt + 1
        call logger%log( '✓' )
      else
        nft = nft + 1
        write ( exp, '(A6,2X,A10,T1)' ) new_line('a'), 'expect'
        write ( got, '(A6,2X,A10,T1)' ) new_line('a'), trim(test%op)

        call logger%err( exp, test%val, got, [ test%exp ] )

        passed = .false.
      end if

      test => test%next
      call logger%end_section
    end do

    call logger%end_section


    if ( passed ) then
      write( msg, '(I0,A,I0,A)' ) npt, ' out of ', nft + npt, ' test(s) passed.'
      call logger%log( msg )
    else
      write( msg, '(I0,A,I0,A)' ) nft, ' out of ', nft + npt, ' test(s) failed.'
      call logger%err( msg )
    end if

  end function rhyme_assertion_passed
end submodule rhyme_assertion_passed_submodule
