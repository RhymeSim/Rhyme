module rhyme_string
  implicit none

  type string_constants_t
    character ( len=16 ) :: int_fmt = '(I0)'
    character ( len=16 ) :: real_fmt = '(ES10.3)'
    character ( len=16 ) :: double_fmt = '(ES12.5)'
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
      character ( len=256 ) :: str
    end function rhyme_string_to_string

    pure elemental module function rhyme_string_is_nan ( input ) result ( is_nan )
      class (*), intent ( in ) :: input
      logical :: is_nan
    end function rhyme_string_is_nan

    pure elemental module function rhyme_strin_get_filename ( path ) result ( filename )
      character ( len=* ), intent ( in ) :: path
      character ( len=256 ) :: filename
    end function rhyme_strin_get_filename
  end interface


  interface operator ( .toString. )
    procedure rhyme_string_to_string
    procedure rhyme_string_array_to_string
  end interface operator ( .toString. )

  interface operator ( .isNaN. )
    procedure rhyme_string_is_nan
  end interface operator ( .isNaN. )

  interface operator ( .filename. )
    procedure rhyme_strin_get_filename
  end interface operator ( .filename. )
end module rhyme_string
