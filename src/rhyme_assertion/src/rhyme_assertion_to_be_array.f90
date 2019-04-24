submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_submodule
contains
  pure module function rhyme_assertion_to_be_array ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:), expect(:)
    type ( test_t ) :: test

    logical :: passed
    integer :: idx

    passed = .false.
    test%op = 'to_be'

    test%exp = .toString. expect
    test%val = .toString. val

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int_arr

      select type ( e => expect )
      type is ( integer )
        idx = maxloc( v - e, dim=1 )
        test%real_val = real( v(idx), kind=8 )
        test%real_exp = real( e(idx), kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( v .eq. e )
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real_arr

      select type ( e => expect )
      type is ( real( kind=4 ) )
        idx = maxloc( v - e, dim=1 )
        test%real_val = real( v(idx), kind=8 )
        test%real_exp = real( e(idx), kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( v - e ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double_arr

      select type ( e => expect )
      type is ( real( kind=8 ) )
        idx = maxloc( v - e, dim=1 )
        test%real_val = v(idx)
        test%real_exp = e(idx)
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( v - e ) < epsilon(0.d0) )
      type is ( real( kind=4 ) )
        ! TODO: set a warning
        idx = maxloc( v - e, dim=1 )
        test%real_val = real( v(idx), kind=8 )
        test%real_exp = real( e(idx), kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( real(v, kind=4) - e ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char_arr

      select type ( e => expect )
      type is ( character(*) )
        passed = all( v .eq. e )
      class default
        passed = .false.
      end select

    type is ( logical )
      test%type = assertid%log_arr

      select type ( e => expect )
      type is ( logical )
        passed = all( v .eqv. e )
      class default
        passed = .false.
      end select

    class default
      test%type = assertid%unset
      passed = .false.
    end select

    test%is_passed = passed
  end function rhyme_assertion_to_be_array
end submodule rhyme_assertion_to_be_array_submodule
