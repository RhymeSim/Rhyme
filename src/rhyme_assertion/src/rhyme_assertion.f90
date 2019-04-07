module rhyme_assertion
  implicit none

  type assertion_indices_t
    integer :: int = 1, real = 2, double = 3, char = 4
    integer :: int_arr = 11, real_arr = 12, double_arr = 13, char_arr = 14
    integer :: unset = -1
  end type assertion_indices_t

  type ( assertion_indices_t ), parameter :: assertid = assertion_indices_t()

  type test_t
    integer :: type = assertid%unset
    logical :: is_passed = .false.
    character ( len=128 ) :: msg = ''
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

    module function rhyme_assertion_is_equal_to ( val, exp ) result ( test )
      class (*), intent ( in ) :: val, exp
      type ( test_t ) :: test
    end function rhyme_assertion_is_equal_to
  end interface


  interface operator ( .describe. )
    procedure rhyme_assertion_describe
  end interface operator ( .describe. )

  interface operator ( .isEqualTo. )
    procedure rhyme_assertion_is_equal_to
  end interface operator ( .isEqualTo. )
end module rhyme_assertion
