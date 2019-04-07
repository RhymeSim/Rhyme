module rhyme_assertion
  implicit none

  type assertion_indices_t
    integer :: int = 1, real = 2, double = 3, char = 4
    integer :: int_arr = 11, real_arr = 12, double_arr = 13, char_arr = 14
    integer :: unset = -1
  end type assertion_indices_t

  type ( assertion_indices_t ), parameter :: assertid = assertion_indices_t()


  type assertion_constants_t
    character ( len=16 ) :: int_fmt = '(I0)'
    character ( len=16 ) :: real_fmt = '(E7.7)'
    character ( len=16 ) :: double_fmt = '(E15.15)'
  end type assertion_constants_t

  type ( assertion_constants_t ), parameter :: assertcnst = assertion_constants_t()


  type test_t
    integer :: type = assertid%unset
    logical :: is_passed = .false.
    character ( len=128 ) :: msg = ''
    character ( len=128 ) :: val = '', op = '', exp = ''
    type ( test_t ), pointer :: next => null()
  end type test_t

  type assertion_t
    character ( len=128 ) :: desc = ''
    type ( test_t ), pointer :: tests => null()
  contains
    procedure :: expect => rhyme_assertion_expect
  end type assertion_t

  interface
    pure module function rhyme_assertion_describe ( desc ) result ( tester )
      character ( len=* ), intent ( in ) :: desc
      type ( assertion_t ) :: tester
    end function rhyme_assertion_describe

    pure module subroutine rhyme_assertion_expect ( this, test )
      class ( assertion_t ), intent ( inout ) :: this
      type ( test_t ), intent ( in ) :: test
    end subroutine rhyme_assertion_expect

    pure module function rhyme_assertion_to_be ( val, exp ) result ( test )
      class (*), intent ( in ) :: val, exp
      type ( test_t ) :: test
    end function rhyme_assertion_to_be

    pure module function rhyme_assertion_add_test_message ( t, msg ) result ( test )
      type ( test_t ), intent ( in ) :: t
      character ( len=* ), intent ( in ) :: msg
      type ( test_t ) :: test
    end function rhyme_assertion_add_test_message
  end interface


  interface operator ( .describe. )
    procedure rhyme_assertion_describe
  end interface operator ( .describe. )

  interface operator ( .toBe. )
    procedure rhyme_assertion_to_be
  end interface operator ( .toBe. )

  interface operator ( .when. )
    procedure rhyme_assertion_add_test_message
  end interface operator ( .when. )
end module rhyme_assertion
