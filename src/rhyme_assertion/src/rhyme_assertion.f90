module rhyme_assertion
  implicit none

  type assertion_indices_t
    integer :: int = 1, real = 2, double = 3, char = 4, log = 5
    integer :: int_arr = 11, real_arr = 12, double_arr = 13, char_arr = 14, log_arr = 15
    integer :: unset = -1
  end type assertion_indices_t

  type ( assertion_indices_t ), parameter :: assertid = assertion_indices_t()


  type assertion_constants_t
    character ( len=16 ) :: int_fmt = '(I0)'
    character ( len=16 ) :: real_fmt = '(E13.7)'
    character ( len=16 ) :: double_fmt = '(E21.15)'
  end type assertion_constants_t

  type ( assertion_constants_t ), parameter :: assertcnst = assertion_constants_t()


  type test_t
    integer :: type = assertid%unset
    logical :: is_passed = .false.
    character ( len=128 ) :: msg = ''
    character ( len=2048 ) :: val = '', op = '', exp = ''
    type ( test_t ), pointer :: next => null()
  end type test_t

  type assertion_t
    character ( len=128 ) :: desc = ''
    type ( test_t ), pointer :: tests => null()
  contains
    procedure :: expect => rhyme_assertion_expect
    procedure :: passed => rhyme_assertion_passed
    procedure :: failed => rhyme_assertion_failed
    procedure :: reset => rhyme_assertion_reset
  end type assertion_t

  interface
    module subroutine rhyme_assertion_reset ( this )
      class ( assertion_t ), intent ( inout ) :: this
    end subroutine rhyme_assertion_reset

    pure module function rhyme_assertion_describe ( desc ) result ( tester )
      character ( len=* ), intent ( in ) :: desc
      type ( assertion_t ) :: tester
    end function rhyme_assertion_describe

    pure module subroutine rhyme_assertion_expect ( this, test )
      class ( assertion_t ), intent ( inout ) :: this
      type ( test_t ), intent ( in ) :: test
    end subroutine rhyme_assertion_expect

    pure module function rhyme_assertion_add_test_message ( t, msg ) result ( test )
      type ( test_t ), intent ( in ) :: t
      character ( len=* ), intent ( in ) :: msg
      type ( test_t ) :: test
    end function rhyme_assertion_add_test_message

    pure module function rhyme_assertion_to_be ( val, exp ) result ( test )
      class (*), intent ( in ) :: val, exp
      type ( test_t ) :: test
    end function rhyme_assertion_to_be

    pure module function rhyme_assertion_to_be_array ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect(:)
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_array

    pure module function rhyme_assertion_to_be_array_scalar ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_array_scalar

    pure module function rhyme_assertion_to_be_array_2d ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:,:), expect(:,:)
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_array_2d

    pure module function rhyme_assertion_not_to_be ( val, exp ) result ( test )
      class (*), intent ( in ) :: val, exp
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be

    pure module function rhyme_assertion_not_to_be_array ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect(:)
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_array

    pure module function rhyme_assertion_not_to_be_array_scalar ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_array_scalar

    logical module function rhyme_assertion_passed ( this ) result ( passed )
      class ( assertion_t ), intent ( in ) :: this
    end function rhyme_assertion_passed

    logical module function rhyme_assertion_failed ( this ) result ( failed )
      class ( assertion_t ), intent ( in ) :: this
    end function rhyme_assertion_failed
  end interface


  interface operator ( .describe. )
    procedure rhyme_assertion_describe
  end interface operator ( .describe. )

  interface operator ( .toBe. )
    procedure rhyme_assertion_to_be
    procedure rhyme_assertion_to_be_array
    procedure rhyme_assertion_to_be_array_scalar
    procedure rhyme_assertion_to_be_array_2d
  end interface operator ( .toBe. )

  interface operator ( .notToBe. )
    procedure rhyme_assertion_not_to_be
    procedure rhyme_assertion_not_to_be_array
    procedure rhyme_assertion_not_to_be_array_scalar
  end interface operator ( .notToBe. )

  interface operator ( .hint. )
    procedure rhyme_assertion_add_test_message
  end interface operator ( .hint. )

  interface arr2str
    procedure array_to_string
    procedure array_2d_to_string
  end interface arr2str

contains

  pure function array_to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input(:)
    character ( len=2048 ) :: str

    integer :: i, length
    character ( len=64 ) :: inp_str

    length = size( input )
    str = ''

    select type ( inp => input )
    type is ( integer )
      do i = 1, length
        write ( inp_str, assertcnst%int_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( real( kind=4 ) )
      do i = 1, length
        write ( inp_str, assertcnst%real_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( real( kind=8 ) )
      do i = 1, length
        write ( inp_str, assertcnst%double_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( character (*) )
      do i = 1, length
        str = trim( str )//'  '//trim(adjustl( inp(i) ) )
      end do
    type is ( logical )
      do i = 1, length
        if ( inp(i) ) then
          str = trim( str )//'  '//'.true.'
        else
          str = trim( str )//'  '//'.false.'
        end if
      end do
    class default
      do i = 1, length
          str = trim( str )//'  '//'unknown type'
      end do
    end select

    if ( length > 1 ) then
      str = '[ '//trim(adjustl( str ) )//' ]'
    else
      str = adjustl( str )
    end if
  end function array_to_string


  pure function array_2d_to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input(:,:)
    character ( len=2048 ) :: str

    integer :: l

    l = product( shape( input ) )

    select type ( inp => input )
    type is ( integer )
      str = array_to_string( reshape( inp, [ l ] ) )
    type is ( real( kind=4 ) )
      str = array_to_string( reshape( inp, [ l ] ) )
    type is ( real( kind=8 ) )
      str = array_to_string( reshape( inp, [ l ] ) )
    type is ( character (*) )
      str = array_to_string( reshape( inp, [ l ] ) )
    type is ( logical )
      str = array_to_string( reshape( inp, [ l ] ) )
    class default
      str = 'unknown type for 2D array'
    end select
  end function array_2d_to_string

end module rhyme_assertion
