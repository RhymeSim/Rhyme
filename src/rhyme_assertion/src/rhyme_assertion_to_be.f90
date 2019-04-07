submodule ( rhyme_assertion ) rhyme_assertion_to_be_submodule
contains
  pure module function rhyme_assertion_to_be ( val, exp ) result ( test )
    implicit none

    class (*), intent ( in ) :: val, exp
    type ( test_t ) :: test

    logical :: passed
    passed = .false.

    test%op = 'to_be'

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int
      write ( test%val, assertcnst%int_fmt ) v

      select type ( e => exp )
      type is ( integer )
        write ( test%exp, assertcnst%int_fmt ) e
        passed = v .eq. e
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real
      write ( test%val, assertcnst%real_fmt ) v

      select type ( e => exp )
      type is ( real( kind=4 ) )
        passed = abs( v - e ) < epsilon(0.e0)
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double
      write ( test%val, assertcnst%double_fmt ) v

      select type ( e => exp )
      type is ( real( kind=8 ) )
        passed = abs( v - e ) < epsilon(0.d0)
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char
      test%val = trim( adjustl( v ) )

      select type ( e => exp )
      type is ( character(*) )
        passed = trim( adjustl( v ) ) .eq. trim( adjustl( e ) )
      class default
        passed = .false.
      end select

    end select


    select type ( e => exp )
    type is ( integer )
      write ( test%exp, assertcnst%int_fmt ) e
    type is ( real( kind=4 ) )
      write ( test%exp, assertcnst%real_fmt ) e
    type is ( real( kind=8 ) )
      write ( test%exp, assertcnst%double_fmt ) e
    type is ( character(*) )
      test%exp = trim( adjustl( e ) )
    class default
      test%exp = 'Unsupported type'
    end select

    test%is_passed = passed
  end function rhyme_assertion_to_be
end submodule rhyme_assertion_to_be_submodule
