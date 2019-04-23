submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_3d_scalar_submodule
contains
  pure module function rhyme_assertion_to_be_array_3d_scalar ( val, expect ) result ( test )
    implicit none

    class (*), intent ( in ) :: val(:,:,:), expect
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l

    l = product( shape( val ) )

    select type ( v => val )
    type is ( integer )
      temp = rhyme_assertion_to_be_array_scalar( reshape(v, [l]), expect )
    type is ( real( kind=4 ) )
      temp = rhyme_assertion_to_be_array_scalar( reshape(v, [l]), expect )
    type is ( real( kind=8 ) )
      temp = rhyme_assertion_to_be_array_scalar( reshape(v, [l]), expect )
    type is ( character(*) )
      temp = rhyme_assertion_to_be_array_scalar( reshape(v, [l]), expect )
    type is ( logical )
      temp = rhyme_assertion_to_be_array_scalar( reshape(v, [l]), expect )
    class default
      temp%type = assertid%unset
      temp%is_passed = .false.
      temp%val = 'Unknown 3D array type'
      temp%op = 'to_be'
    end select

    call temp%copy_to( test )
  end function rhyme_assertion_to_be_array_3d_scalar
end submodule rhyme_assertion_to_be_array_3d_scalar_submodule
