module rhyme_assertion
  implicit none

  type assertion_indices_t
    integer :: nan = 0, int = 1, real = 2, double = 3, char = 4, log = 5, unknown = 6
    integer :: int_arr = 11, real_arr = 12, double_arr = 13, char_arr = 14, log_arr = 15
    integer :: unset = -1
  end type assertion_indices_t

  type ( assertion_indices_t ), parameter :: assertid = assertion_indices_t()

  character ( len=16 ), parameter :: unknown_type_str = 'UnknownType'


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
    real ( kind=8 ) :: within = 0.d0
    real ( kind=8 ) :: real_accuracy = 0.d0
    real ( kind=8 ) :: real_val = 0.d0
    real ( kind=8 ) :: real_exp = 0.d0
    type ( test_t ), pointer :: next => null()
  contains
    procedure :: copy_to => rhyme_assertion_test_copy_to
    procedure :: copy_essentials_to => rhyme_assertion_test_copy_essentials_to
    procedure :: set_type => rhyme_assertion_test_set_type
    procedure :: set_real_val => rhyme_assertion_test_set_real_val
  end type test_t

  type assertion_t
    character ( len=128 ) :: desc = ''
    type ( test_t ), pointer :: tests => null()
    type ( test_t ), pointer :: tail => null()
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

    pure module function rhyme_assertion_to_be_nan_array ( input ) result ( test )
      class (*), intent ( in ) :: input(:)
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_nan_array

    elemental pure module function rhyme_assertion_to_be_nan ( input ) result ( test )
      class (*), intent ( in ) :: input
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_nan

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

    pure module function rhyme_assertion_to_be_array_3d ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:,:,:), expect(:,:,:)
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_array_3d

    pure module function rhyme_assertion_to_be_array_3d_scalar ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:,:,:), expect
      type ( test_t ) :: test
    end function rhyme_assertion_to_be_array_3d_scalar

    pure module function rhyme_assertion_not_to_be ( val, exp ) result ( test )
      class (*), intent ( in ) :: val, exp
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be

    elemental pure module function rhyme_assertion_not_to_be_nan ( input ) result ( test )
      class (*), intent ( in ) :: input
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_nan

    pure module function rhyme_assertion_not_to_be_nan_array ( input ) result ( test )
      class (*), intent ( in ) :: input(:)
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_nan_array

    pure module function rhyme_assertion_not_to_be_array ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect(:)
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_array

    pure module function rhyme_assertion_not_to_be_array_scalar ( val, expect ) result ( test )
      class (*), intent ( in ) :: val(:), expect
      type ( test_t ) :: test
    end function rhyme_assertion_not_to_be_array_scalar

    pure module function rhyme_assertion_within ( test, accuracy ) result ( ntest )
      type ( test_t ), intent ( in ) :: test
      class (*), intent ( in ) :: accuracy
      type ( test_t ) :: ntest
    end function rhyme_assertion_within

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
    procedure rhyme_assertion_to_be_array_3d
    procedure rhyme_assertion_to_be_array_3d_scalar
  end interface operator ( .toBe. )

  interface operator ( .toBeNaN. )
    procedure rhyme_assertion_to_be_nan
    procedure rhyme_assertion_to_be_nan_array
  end interface operator ( .toBeNaN. )

  interface operator ( .notToBe. )
    procedure rhyme_assertion_not_to_be
    procedure rhyme_assertion_not_to_be_array
    procedure rhyme_assertion_not_to_be_array_scalar
  end interface operator ( .notToBe. )

  interface operator ( .notToBeNaN. )
    procedure rhyme_assertion_not_to_be_nan
    procedure rhyme_assertion_not_to_be_nan_array
  end interface operator ( .notToBeNaN. )

  interface operator ( .within. )
    procedure rhyme_assertion_within
  end interface operator ( .within. )

  interface operator ( .hint. )
    procedure rhyme_assertion_add_test_message
  end interface operator ( .hint. )

  interface operator ( .isNaN. )
    procedure rhyme_assertion_is_nan
  end interface operator ( .isNaN. )

  ! TODO: move this to a new module handeling string stuff
  interface operator ( .toString. )
    procedure rhyme_assertion_to_string
    procedure rhyme_assertion_array_to_string
  end interface operator ( .toString. )

contains

  pure subroutine rhyme_assertion_test_copy_essentials_to ( this, test )
    implicit none

    class ( test_t ), intent ( in ) :: this
    type ( test_t ), intent ( out ) :: test

    test%type = this%type
    test%is_passed = this%is_passed
    test%msg = this%msg
    test%val = this%val
    test%op = this%op
    test%exp = this%exp
    test%within = this%within
    test%real_accuracy = this%real_accuracy
    test%real_val = this%real_val
    test%real_exp = this%real_exp
  end subroutine rhyme_assertion_test_copy_essentials_to


  pure subroutine rhyme_assertion_test_copy_to ( this, test )
    implicit none

    class ( test_t ), intent ( inout ) :: this
    type ( test_t ), intent ( inout ) :: test

    call this%copy_essentials_to( test )

    test%next => this%next
  end subroutine rhyme_assertion_test_copy_to


  pure subroutine rhyme_assertion_test_set_type ( this, input )
    implicit none

    class ( test_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: input

    select type ( inp => input )
    type is ( integer )
      this%type = assertid%int
    type is ( real( kind=4 ) )
      this%type = assertid%real
    type is ( real( kind=8 ) )
      this%type = assertid%double
    type is ( character(*) )
      this%type = assertid%char
    type is ( logical )
      this%type = assertid%log
      class default
      this%type = assertid%unknown
    end select
  end subroutine rhyme_assertion_test_set_type


  pure subroutine rhyme_assertion_test_set_real_val ( this, input )
    use, intrinsic :: ieee_arithmetic

    implicit none

    class ( test_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: input

    select type ( inp => input )
    type is ( integer )
      this%real_val = real( inp, kind=8 )
    type is ( real( kind=4 ) )
      this%real_val = real( inp, kind=8 )
    type is ( real( kind=8 ) )
      this%real_val = inp
      class default
      this%real_val = ieee_value( this%real_val, ieee_quiet_nan )
    end select
  end subroutine rhyme_assertion_test_set_real_val


  pure function rhyme_assertion_array_to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input(:)
    character ( len=2048 ) :: str

    character ( len=32 ) :: ch_arr( size(input) )
    integer :: i

    str = ''
    ch_arr = rhyme_assertion_to_string( input )

    do i = 1, size(input)
      str = trim( adjustl(str) ) // ' ' // trim( adjustl(ch_arr(i)) )
    end do

    str = '[ ' // trim( adjustl(str) ) // ' ]'
  end function rhyme_assertion_array_to_string


  elemental pure function rhyme_assertion_to_string ( input ) result ( str )
    use, intrinsic :: ieee_arithmetic

    implicit none

    class (*), intent ( in ) :: input
    character ( len=32 ) :: str

    if ( rhyme_assertion_is_nan( input ) ) then
        str = 'NaN'
    else
      select type ( inp => input )
      type is ( integer )
        write ( str, assertcnst%int_fmt ) inp
      type is ( real( kind=4 ) )
        write ( str, assertcnst%real_fmt ) inp
      type is ( real( kind=8 ) )
        write ( str, assertcnst%double_fmt ) inp
      type is ( character (*) )
        str = "'" // trim( adjustl(inp) ) // "'"
      type is ( logical )
        if ( inp ) then
          str = '.true.'
        else
          str = '.false.'
        end if
      class default
        str = unknown_type_str
      end select
    end if
  end function rhyme_assertion_to_string


  elemental pure logical function rhyme_assertion_is_nan ( input ) result ( is_nan )
    use, intrinsic :: ieee_arithmetic

    implicit none

    class (*), intent ( in ) :: input

    select type ( inp => input )
    type is ( real( kind=4 ) )
      if( ieee_is_nan(inp) ) then
        is_nan = .true.
      else
        is_nan = .false.
      end if

    type is ( real( kind=8 ) )
      if( ieee_is_nan(inp) ) then
        is_nan = .true.
      else
        is_nan = .false.
      end if

    class default
      is_nan = .false.
    end select
  end function rhyme_assertion_is_nan
end module rhyme_assertion
