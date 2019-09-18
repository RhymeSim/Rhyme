submodule ( rhyme_assertion ) rhyme_assertion_to_be_submodule
contains
  pure module function rhyme_assertion_to_be_ii ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val, exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%int

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = real( exp, kind=8 )
    test%is_passed = val .eq. exp
  end function rhyme_assertion_to_be_ii

  pure module function rhyme_assertion_to_be_ir ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val
    real ( kind=4 ), intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%int

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = real( exp, kind=8 )

    if ( abs( int(exp) - exp ) > epsilon(0e0) ) then
      test%is_passed = .false.
    else
      test%is_passed = val .eq. int( exp )
    end if
  end function rhyme_assertion_to_be_ir

  pure module function rhyme_assertion_to_be_ir8 ( val, exp ) result ( test )
    implicit none

    integer, intent ( in ) :: val
    real ( kind=8 ), intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%int

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = exp

    if ( abs( int(exp) - exp ) > epsilon(0d0) ) then
      test%is_passed = .false.
    else
      test%is_passed = val .eq. int( exp )
    end if
  end function rhyme_assertion_to_be_ir8

  pure function rhyme_assertion_to_be_ri ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val
    integer, intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%real

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = real( exp, kind=8 )

    if ( abs( int(val) - val ) > epsilon(0e0) ) then
      test%is_passed = .false.
    else
      test%is_passed = int(val) .eq. exp
    end if
  end function rhyme_assertion_to_be_ri

  pure function rhyme_assertion_to_be_rr ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%real

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = real( exp, kind=8 )

    test%is_passed = abs( val - exp ) < epsilon(0e0)
  end function rhyme_assertion_to_be_rr

  pure function rhyme_assertion_to_be_rr8 ( val, exp ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: val
    real ( kind=8 ), intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%real

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = real( val, kind=8 )
    test%real_exp = exp

    test%is_passed = abs( val - exp ) < epsilon(0e0)
  end function rhyme_assertion_to_be_rr8

  pure function rhyme_assertion_to_be_r8i ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val
    integer, intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%double

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = val
    test%real_exp = real( exp, kind=8 )

    if ( abs( int(val) - val ) > epsilon(0d0) ) then
      test%is_passed = .false.
    else
      test%is_passed = int(val) .eq. exp
    end if
  end function rhyme_assertion_to_be_r8i

  pure function rhyme_assertion_to_be_r8r ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val
    real ( kind=4 ), intent ( in ) :: exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%double

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( real( val - exp, kind=8 ) )
    test%real_val = val
    test%real_exp = real( exp, kind=8 )

    test%is_passed = abs( val - exp ) < epsilon(0d0)
  end function rhyme_assertion_to_be_r8r

  pure function rhyme_assertion_to_be_r8r8 ( val, exp ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%double

    test%val = .toString. val
    test%exp = .toString. exp

    test%real_accuracy = abs( val - exp )
    test%real_val = val
    test%real_exp = exp

    test%is_passed = abs( val - exp ) < epsilon(0d0)
  end function rhyme_assertion_to_be_r8r8

  pure function rhyme_assertion_to_be_chch ( val, exp ) result ( test )
    implicit none

    character ( len=* ), intent ( in ) :: val, exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%char

    test%val = trim( val )
    test%exp = trim( exp )

    test%is_passed = adjustl( trim( val ) ) .eq. adjustl( trim( exp ) )
  end function rhyme_assertion_to_be_chch

  pure function rhyme_assertion_to_be_ll ( val, exp ) result ( test )
    implicit none

    logical, intent ( in ) :: val, exp
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%log

    test%val = .toString. val
    test%exp = .toString. exp

    test%is_passed = val .eqv. exp
  end function rhyme_assertion_to_be_ll
end submodule rhyme_assertion_to_be_submodule
