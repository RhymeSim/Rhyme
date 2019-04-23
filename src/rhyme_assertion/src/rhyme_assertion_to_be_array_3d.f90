submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_3d_submodule
contains
  pure module function rhyme_assertion_to_be_array_3d ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:,:,:), expect(:,:,:)
    type ( test_t ) :: test

    logical :: passed
    integer :: l
    passed = .false.

    test%op = 'to_be'
    l = product( shape( val ) )

    select type ( v => val )
    type is ( integer )
      test%type = assertid%int_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( integer )
        passed = all( reshape( v, [l] ) .eq. reshape( e, [l] ) )
      class default
        passed = .false.
      end select

    type is ( real( kind=4 ) )
      test%type = assertid%real_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=4 ) )
        passed = all( abs( reshape( v, [l] ) - reshape( e, [l] ) ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( real( kind=8 ) )
      test%type = assertid%double_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( real( kind=8 ) )
        passed = all( abs( reshape( v, [l] ) - reshape( e, [l] ) ) < epsilon(0.d0) )
      type is ( real( kind=4 ) )
        ! TODO: set a warning
        passed = all( abs( reshape( real(v, kind=4), [l] ) - reshape( e, [l] ) ) < epsilon(0.e0) )
      class default
        passed = .false.
      end select

    type is ( character(*) )
      test%type = assertid%char_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( character(*) )
        passed = all( reshape( v, [l] ) .eq. reshape( e, [l] ) )
      class default
        passed = .false.
      end select

    type is ( logical )
      test%type = assertid%log_arr
      test%val = arr2str( v )

      select type ( e => expect )
      type is ( logical )
        passed = all( reshape( v, [l] ) .eqv. reshape( e, [l] ) )
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

  end function rhyme_assertion_to_be_array_3d
end submodule rhyme_assertion_to_be_array_3d_submodule
