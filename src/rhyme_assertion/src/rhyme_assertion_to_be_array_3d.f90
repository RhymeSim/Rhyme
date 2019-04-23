submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_3d_submodule
contains
  pure module function rhyme_assertion_to_be_array_3d ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:,:,:), expect(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: lv, le

    lv = product( shape( val ) )
    le = product( shape( expect ) )

    temp%type = assertid%unset
    temp%op = 'to_be'

    select type ( v => val )
    type is ( integer )
      select type ( e => expect )
      type is ( integer )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), reshape(e, [le]))
      class default
        temp%is_passed = .false.
        temp%val = 'Integer 3D array'
        temp%exp = 'Unmatched 3D array'
      end select

    type is ( real( kind=4 ) )
      select type ( e => expect )
      type is ( real( kind=4 ) )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), reshape(e, [le]) )
      class default
        temp%is_passed = .false.
        temp%val = 'Real 3D array'
        temp%exp = 'Unmatched 3D array'
      end select

    type is ( real( kind=8 ) )
      select type ( e => expect )
      type is ( real( kind=8 ) )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), reshape(e, [le]) )
      type is ( real( kind=4 ) )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), real(reshape(e, [le]), kind=4) )
      class default
        temp%is_passed = .false.
        temp%val = 'Double 3D array'
        temp%exp = 'Unmatched 3D array'
      end select

    type is ( character(*) )
      select type ( e => expect )
      type is ( character(*) )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), reshape(e, [le]) )
      class default
        temp%is_passed = .false.
        temp%val = 'Character 3D array'
        temp%exp = 'Unmatched 3D array'
      end select

    type is ( logical )
      select type ( e => expect )
      type is ( logical )
        temp = rhyme_assertion_to_be_array( reshape(v, [lv]), reshape(e, [le]) )
      class default
        temp%is_passed = .false.
        temp%val = 'Logical 3D array'
        temp%exp = 'Unmatched 3D array'
      end select

    class default
      temp%type = assertid%unset
      temp%is_passed = .false.
      temp%val = 'Unknown 3D array type'
      temp%op = 'to_be'
      temp%val = 'Unknown array'
    end select

    call temp%copy_to( test )
  end function rhyme_assertion_to_be_array_3d
end submodule rhyme_assertion_to_be_array_3d_submodule
