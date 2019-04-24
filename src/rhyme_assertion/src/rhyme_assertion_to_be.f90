submodule ( rhyme_assertion ) rhyme_assertion_to_be_submodule
contains
  pure module function rhyme_assertion_to_be ( val, exp ) result ( test )
    implicit none

    class (*), intent ( in ) :: val, exp
    type ( test_t ) :: test

    logical :: passed

    passed = .false.
    test%op = 'to_be'

    test%val = .toString. val
    test%exp = .toString. exp

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int

      select type ( e => exp )
      type is ( integer )
        test%real_accuracy = abs( real( v - e, kind=8 ) )
        test%real_val = real( v, kind=8 )
        test%real_exp = real( e, kind=8 )
        passed = v .eq. e
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real

      select type ( e => exp )
      type is ( real( kind=4 ) )
        test%real_accuracy = abs( v - e )
        test%real_val = real( v, kind=8 )
        test%real_exp = real( e, kind=8 )
        passed = test%real_accuracy < epsilon(0.e0)
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double

      select type ( e => exp )
      type is ( real( kind=8 ) )
        test%real_accuracy = abs( v - e )
        test%real_val = v
        test%real_exp = e
        passed = test%real_accuracy < epsilon(0.d0)
      type is ( real( kind=4 ) )
        ! TODO: set a warning
        test%real_accuracy = abs( real(v, kind=4) - e )
        test%real_val = real( v, kind=8 )
        test%real_exp = real( e, kind=8 )
        passed = test%real_accuracy < epsilon(0.e0)
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char

      select type ( e => exp )
      type is ( character(*) )
        passed = trim( adjustl( v ) ) .eq. trim( adjustl( e ) )
      class default
        passed = .false.
      end select

    type is ( logical )
      test%type = assertid%log

      select type ( e => exp )
      type is ( logical )
        passed = v .eqv. e
      class default
        passed = .false.
      end select

    class default
      test%type = assertid%unset
      passed = .false.
    end select

    test%is_passed = passed
  end function rhyme_assertion_to_be
end submodule rhyme_assertion_to_be_submodule
