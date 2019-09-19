submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_submodule
contains
  pure module function rhyme_assertion_to_be_array_ii ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:), arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    test%is_passed = all( arr1 .eq. arr2 )
  end function rhyme_assertion_to_be_array_ii

  pure module function rhyme_assertion_to_be_array_ir ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:)
    real ( kind=4 ), intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    if ( any( abs( int(arr2) - arr2 ) > epsilon(0e0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( arr1 .eq. int(arr2) )
    end if
  end function rhyme_assertion_to_be_array_ir

  pure module function rhyme_assertion_to_be_array_ir8 ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:)
    real ( kind=8 ), intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = arr2(idx)
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    if ( any( abs( int(arr2) - arr2 ) > epsilon(0d0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( arr1 .eq. int(arr2) )
    end if
  end function rhyme_assertion_to_be_array_ir8

  pure module function rhyme_assertion_to_be_array_ri ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:)
    integer, intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%real_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    if ( any( abs( int(arr1) - arr1 ) > epsilon(0e0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( int(arr1) .eq. arr2 )
    end if
  end function rhyme_assertion_to_be_array_ri

  pure module function rhyme_assertion_to_be_array_rr ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:), arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%real_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    test%is_passed = all( abs(arr1 - arr2) < epsilon(0e0) )
  end function rhyme_assertion_to_be_array_rr

  pure module function rhyme_assertion_to_be_array_rr8 ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:)
    real ( kind=8 ), intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%int_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = real( arr1(idx), kind=8 )
    test%real_exp = arr2(idx)
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    test%is_passed = all( abs(arr1 - arr2) < epsilon(0e0) )
  end function rhyme_assertion_to_be_array_rr8

  pure module function rhyme_assertion_to_be_array_r8i ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:)
    integer, intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = arr1(idx)
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    if ( any( abs( int(arr1) - arr1 ) > epsilon(0d0) ) ) then
      test%is_passed = .false.
    else
      test%is_passed = all( int(arr1) .eq. arr2 )
    end if
  end function rhyme_assertion_to_be_array_r8i

  pure module function rhyme_assertion_to_be_array_r8r ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:)
    real ( kind=4 ), intent ( in ) :: arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = arr1(idx)
    test%real_exp = real( arr2(idx), kind=8 )
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    test%is_passed = all( abs(arr1 - arr2) < epsilon(0e0) )
  end function rhyme_assertion_to_be_array_r8r

  pure module function rhyme_assertion_to_be_array_r8r8 ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:), arr2(:)
    type ( test_t ) :: test

    integer :: idx

    test%op = 'to_be'
    test%type = assertid%double_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    idx = maxloc( abs( arr1 - arr2 ), dim=1 )
    test%real_val = arr1(idx)
    test%real_exp = arr2(idx)
    test%real_accuracy = abs( arr1(idx) - arr2(idx) )

    test%is_passed = all( abs(arr1 - arr2) < epsilon(0d0) )
  end function rhyme_assertion_to_be_array_r8r8

  pure module function rhyme_assertion_to_be_array_chch ( arr1, arr2 ) result ( test )
    implicit none

    character ( len=* ), intent ( in ) :: arr1(:), arr2(:)
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%char_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    test%is_passed = all( arr1 .eq. arr2 )
  end function rhyme_assertion_to_be_array_chch

  pure module function rhyme_assertion_to_be_array_ll ( arr1, arr2 ) result ( test )
    implicit none

    logical, intent ( in ) :: arr1(:), arr2(:)
    type ( test_t ) :: test

    test%op = 'to_be'
    test%type = assertid%log_arr

    test%val = .toString. arr1
    test%exp = .toString. arr2

    test%is_passed = all( arr1 .eqv. arr2 )
  end function rhyme_assertion_to_be_array_ll
end submodule rhyme_assertion_to_be_array_submodule
