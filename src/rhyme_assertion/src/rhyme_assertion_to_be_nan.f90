submodule ( rhyme_assertion ) rhyme_assertion_to_be_nan_submodule
contains
  elemental pure module function rhyme_assertion_to_be_nan ( input ) result ( test )
    use, intrinsic :: ieee_arithmetic

    implicit none

    class (*), intent ( in ) :: input
    type ( test_t ) :: test

    test%op = 'to_be_nan'

    test%real_exp = ieee_value( test%real_exp, ieee_quiet_nan )
    test%real_accuracy = ieee_value( test%real_accuracy, ieee_quiet_nan )
    test%within = ieee_value( test%within, ieee_quiet_nan )

    test%val = .toString. input
    test%exp = 'NaN'

    call test%set_type( input )
    call test%set_real_val( input )

    if ( .isNaN. input ) then
      test%is_passed = .true.
    else
      test%is_passed = .false.
    end if
  end function rhyme_assertion_to_be_nan
end submodule rhyme_assertion_to_be_nan_submodule
