submodule ( rhyme_assertion ) rhyme_assertion_passed_submodule
contains
  logical module function rhyme_assertion_passed ( this ) result ( passed )
    use rhyme_log

    implicit none

    class ( assertion_t ), intent ( in ) :: this

    type ( test_t ), pointer :: test
    type ( log_t ) :: log

    integer :: nft, npt
    character ( len=128 ) :: err_msg

    call log%set_section( this%desc )

    passed = .true.
    nft = 0
    npt = 0

    test => this%tests

    if ( .not. associated( test ) ) then
      call log%warn( 'Nothing to test.' )
      return
    end if

    do while ( associated( test ) )
      if ( .not. test%is_passed ) then
        passed = .false.
        nft = nft + 1
        call log%err( 'expected:', test%val, test%op, [ test%exp ] )
        test => test%next
        cycle
      end if

      npt = npt + 1
      call log%log( trim( test%msg )//' has been passed.' )

      test => test%next
    end do

    call log%set_section( '' )

    if ( passed ) then
      call log%log( 'All tests have been passed.', '#tests', ':', [ npt ] )
    else
      write( err_msg, '(I0,A,I0,A)' ) nft, ' out of ', nft + npt, ' tests failed.'
      call log%err( err_msg )
    end if
  end function rhyme_assertion_passed
end submodule rhyme_assertion_passed_submodule
