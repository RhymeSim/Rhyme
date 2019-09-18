submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_scalar_submodule
contains
  pure module function rhyme_assertion_to_be_array_scalar_ii ( arr, scalar ) result ( test )
    implicit none

    integer, intent ( in ) :: arr(:), scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    test%is_passed = all( arr .eq. scalar )
  end function rhyme_assertion_to_be_array_scalar_ii

  pure module function rhyme_assertion_to_be_array_scalar_ir ( arr, scalar ) result ( test )
    implicit none

    integer, intent ( in ) :: arr(:)
    real ( kind=4 ), intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    if ( abs( int(scalar) - scalar ) > epsilon(0e0) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( arr .eq. int(scalar) )
    end if
  end function rhyme_assertion_to_be_array_scalar_ir

  pure module function rhyme_assertion_to_be_array_scalar_ir8 ( arr, scalar ) result ( test )
    implicit none

    integer, intent ( in ) :: arr(:)
    real ( kind=8 ), intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = scalar
    test%real_accuracy = abs( arr(idx) - scalar )

    if ( abs( int(scalar) - scalar ) > epsilon(0d0) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( arr .eq. int(scalar) )
    end if
  end function rhyme_assertion_to_be_array_scalar_ir8

  pure module function rhyme_assertion_to_be_array_scalar_ri ( arr, scalar ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr(:)
    integer, intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%real_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    if ( any( abs( int(arr) - arr ) > epsilon(0e0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( int(arr) .eq. scalar )
    end if
  end function rhyme_assertion_to_be_array_scalar_ri

  pure module function rhyme_assertion_to_be_array_scalar_rr ( arr, scalar ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr(:), scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%real_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    test%is_passed = all( abs(arr - scalar) < epsilon(0e0) )
  end function rhyme_assertion_to_be_array_scalar_rr

  pure module function rhyme_assertion_to_be_array_scalar_rr8 ( arr, scalar ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr(:)
    real ( kind=8 ), intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = real( arr(idx), kind=8 )
    test%real_exp = scalar
    test%real_accuracy = abs( arr(idx) - scalar )

    test%is_passed = all( abs(arr - scalar) < epsilon(0e0) )
  end function rhyme_assertion_to_be_array_scalar_rr8

  pure module function rhyme_assertion_to_be_array_scalar_r8i ( arr, scalar ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr(:)
    integer, intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = arr(idx)
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    if ( any( abs( int(arr) - arr ) > epsilon(0d0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( int(arr) .eq. scalar )
    end if
  end function rhyme_assertion_to_be_array_scalar_r8i

  pure module function rhyme_assertion_to_be_array_scalar_r8r ( arr, scalar ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr(:)
    real ( kind=4 ), intent ( in ) :: scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = arr(idx)
    test%real_exp = real( scalar, kind=8 )
    test%real_accuracy = abs( arr(idx) - scalar )

    test%is_passed = all( abs(arr - scalar) < epsilon(0d0) )
  end function rhyme_assertion_to_be_array_scalar_r8r

  pure module function rhyme_assertion_to_be_array_scalar_r8r8 ( arr, scalar ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr(:), scalar
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    idx = maxloc( abs( arr - scalar ), dim=1 )
    test%real_val = arr(idx)
    test%real_exp = scalar
    test%real_accuracy = abs( arr(idx) - scalar )

    test%is_passed = all( abs(arr - scalar) < epsilon(0d0) )
  end function rhyme_assertion_to_be_array_scalar_r8r8

  pure module function rhyme_assertion_to_be_array_scalar_chch ( arr, scalar ) result ( test )
    implicit none

    character ( len=* ), intent ( in ) :: arr(:), scalar
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%char_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    test%is_passed = all( arr .eq. scalar )
  end function rhyme_assertion_to_be_array_scalar_chch

  pure module function rhyme_assertion_to_be_array_scalar_ll ( arr, scalar ) result ( test )
    implicit none

    logical, intent ( in ) :: arr(:), scalar
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%log_arr

    test%val = .toString. arr
    test%exp = .toString. scalar

    test%is_passed = all( arr .eqv. scalar )
  end function rhyme_assertion_to_be_array_scalar_ll
end submodule rhyme_assertion_to_be_array_scalar_submodule
