module rhyme_string
  implicit none

  type string_constants_t
    character ( len=16 ) :: int_fmt = '(I0)'
    character ( len=16 ) :: real_fmt = '(E13.7)'
    character ( len=16 ) :: double_fmt = '(E21.15)'
  end type string_constants_t

  type ( string_constants_t ), parameter :: strcnst = string_constants_t()

  character ( len=16 ), parameter :: unknown_type_str = 'UnknownType'

  interface
    pure module function rhyme_string_array_to_string ( input ) result ( str )
      class (*), intent ( in ) :: input(:)
      character ( len=2048 ) :: str
    end function rhyme_string_array_to_string

    pure elemental module function rhyme_string_to_string ( input ) result ( str )
      class (*), intent ( in ) :: input
      character ( len=32 ) :: str
    end function rhyme_string_to_string

    pure elemental module function rhyme_string_is_nan ( input ) result ( is_nan )
      class (*), intent ( in ) :: input
      logical :: is_nan
    end function rhyme_string_is_nan
  end interface


  interface operator ( .toString. )
    procedure rhyme_string_to_string
    procedure rhyme_string_array_to_string
  end interface operator ( .toString. )


  interface operator ( .isNaN. )
    procedure rhyme_string_is_nan
  end interface operator ( .isNaN. )
end module rhyme_string