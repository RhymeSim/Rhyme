submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_scalar_submodule
contains
  pure module function rhyme_assertion_to_be_array_scalar ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:), expect
    type ( test_t ) :: test

    logical :: passed
    integer :: idx

    passed = .false.
    test%op = 'to_be'

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( integer )
        idx = maxloc( v - e, dim=1 )
        test%real_val = real( v(idx), kind=8 )
        test%real_exp = real( e, kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( v .eq. e )
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=4 ) )
        idx = maxloc( v - e, dim=1 )
        test%real_val = real( v(idx), kind=8 )
        test%real_exp = real( e, kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( v - e ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=8 ) )
        idx = maxloc( v - e, dim=1 )
        test%real_val = v(idx)
        test%real_exp = e
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( v - e ) < epsilon(0.d0) )
      type is ( real( kind=4 ) )
        ! TODO: set a warning
        idx = maxloc( v - e, dim=1 )
        test%real_val = v(idx)
        test%real_exp = real( e, kind=8 )
        test%real_accuracy = abs( test%real_val - test%real_exp )
        passed = all( abs( real(v, kind=4) - e ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( character(*) )
        passed = all( v .eq. e )
      class default
        passed = .false.
      end select

    type is ( logical )
      test%type = assertid%log_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( logical )
        passed = all( v .eqv. e )
      class default
        passed = .false.
      end select

    class default
      test%type = assertid%unset
      passed = .false.
      test%val = 'Unsupported type'
    end select


    select type ( e => expect )
    type is ( integer )
      write ( test%exp, assertcnst%int_fmt ) e
    type is ( real( kind=4 ) )
      write ( test%exp, assertcnst%real_fmt ) e
    type is ( real( kind=8 ) )
      write ( test%exp, assertcnst%double_fmt ) e
    type is ( character(*) )
      test%exp = trim( adjustl( e ) )
    type is ( logical )
      if ( e ) then
        test%exp = '.true.'
      else
        test%exp = '.false.'
      end if
    class default
      test%exp = 'Unsupported type'
    end select

    test%is_passed = passed

  end function rhyme_assertion_to_be_array_scalar
end submodule rhyme_assertion_to_be_array_scalar_submodule