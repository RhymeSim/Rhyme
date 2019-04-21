submodule ( rhyme_assertion ) rhyme_assertion_not_to_be_array_submodule
contains
  pure module function rhyme_assertion_not_to_be_array ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:), expect(:)
    type ( test_t ) :: test

    logical :: passed
    passed = .false.

    test%op = 'not_to_be'

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( integer )
        passed = all( v .ne. e )
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=4 ) )
        passed = all( abs( v - e ) > epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=8 ) )
        passed = all( abs( v - e ) > epsilon(0.d0) )
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( character(*) )
        passed = all( v .ne. e )
      class default
        passed = .false.
      end select

    type is ( logical )
      test%type = assertid%log_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( logical )
        passed = all( v .neqv. e )
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
      test%exp = arr2str( e )
    type is ( real( kind=4 ) )
      test%exp = arr2str( e )
    type is ( real( kind=8 ) )
      test%exp = arr2str( e )
    type is ( character(*) )
      test%exp = arr2str( e )
    type is ( logical )
      test%exp = arr2str( e )
    class default
      test%exp = 'Unsupported type'
    end select

    test%is_passed = passed

  end function rhyme_assertion_not_to_be_array
end submodule rhyme_assertion_not_to_be_array_submodule
