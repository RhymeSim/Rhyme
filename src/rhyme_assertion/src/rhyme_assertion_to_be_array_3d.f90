submodule ( rhyme_assertion ) rhyme_assertion_to_be_array_3d_submodule
contains
  pure module function rhyme_assertion_to_be_array_3d_ii ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:,:,:), arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_ii( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%int_arr3d
  end function rhyme_assertion_to_be_array_3d_ii

  pure module function rhyme_assertion_to_be_array_3d_ir ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:,:,:)
    real ( kind=4 ), intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_ir( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%int_arr3d
  end function rhyme_assertion_to_be_array_3d_ir

  pure module function rhyme_assertion_to_be_array_3d_ir8 ( arr1, arr2 ) result ( test )
    implicit none

    integer, intent ( in ) :: arr1(:,:,:)
    real ( kind=8 ), intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_ir8( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%int_arr3d
  end function rhyme_assertion_to_be_array_3d_ir8

  pure module function rhyme_assertion_to_be_array_3d_ri ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:,:,:)
    integer, intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_ri( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%real_arr3d
  end function rhyme_assertion_to_be_array_3d_ri

  pure module function rhyme_assertion_to_be_array_3d_rr ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:,:,:), arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_rr( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%real_arr3d
  end function rhyme_assertion_to_be_array_3d_rr

  pure module function rhyme_assertion_to_be_array_3d_rr8 ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=4 ), intent ( in ) :: arr1(:,:,:)
    real ( kind=8 ), intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_rr8( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%real_arr3d
  end function rhyme_assertion_to_be_array_3d_rr8

  pure module function rhyme_assertion_to_be_array_3d_r8i ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:,:,:)
    integer, intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_r8i( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%double_arr3d
  end function rhyme_assertion_to_be_array_3d_r8i

  pure module function rhyme_assertion_to_be_array_3d_r8r ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:,:,:)
    real ( kind=4 ), intent ( in ) :: arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_r8r( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%double_arr3d
  end function rhyme_assertion_to_be_array_3d_r8r

  pure module function rhyme_assertion_to_be_array_3d_r8r8 ( arr1, arr2 ) result ( test )
    implicit none

    real ( kind=8 ), intent ( in ) :: arr1(:,:,:), arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_r8r8( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%double_arr3d
  end function rhyme_assertion_to_be_array_3d_r8r8

  pure module function rhyme_assertion_to_be_array_3d_chch ( arr1, arr2 ) result ( test )
    implicit none

    character ( len=* ), intent ( in ) :: arr1(:,:,:), arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_chch( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%char_arr3d
  end function rhyme_assertion_to_be_array_3d_chch

  pure module function rhyme_assertion_to_be_array_3d_ll ( arr1, arr2 ) result ( test )
    implicit none

    logical, intent ( in ) :: arr1(:,:,:), arr2(:,:,:)
    type ( test_t ) :: test

    type ( test_t ) :: temp
    integer :: l1, l2

    l1 = product( shape( arr1 ) )
    l2 = product( shape( arr2 ) )

    temp = rhyme_assertion_to_be_array_ll( reshape(arr1, [l1]), reshape(arr2, [l2]))
    call temp%copy_to( test )

    test%op = 'to_be'
    test%type = assertid%log_arr3d
  end function rhyme_assertion_to_be_array_3d_ll
end submodule rhyme_assertion_to_be_array_3d_submodule
