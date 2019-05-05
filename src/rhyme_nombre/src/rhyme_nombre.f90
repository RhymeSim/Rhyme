module rhyme_nombre
  use rhyme_nombre_units

  implicit none

  type nombre_t
    real ( kind=8 ) :: v
    type ( nombre_unit_t ), pointer :: u => null()
    contains
     procedure :: p => rhyme_nombre_print
  end type nombre_t


  interface
    module function rhyme_nombre_new ( val, u ) result ( n )
      class (*), intent ( in ) :: val
      type ( nombre_unit_t ), intent ( in ), target :: u
      type ( nombre_t ) :: n
    end function rhyme_nombre_new

    module function rhyme_nombre_to ( n, u_new ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_unit_t ), pointer, intent ( in ) :: u_new
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_to

    module function rhyme_nombre_mul ( mul, n ) result ( n_new )
      class (*), intent ( in ) :: mul
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_mul

    module function rhyme_nombre_mul_rev ( n, mul ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      class (*), intent ( in ) :: mul
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_mul_rev

    module function rhyme_nombre_div ( n, div ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      class (*), intent ( in ) :: div
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_div

    module function rhyme_nombre_div_rev ( div, n ) result ( n_new )
      class (*), intent ( in ) :: div
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_div_rev

    module function rhyme_nombre_print ( this ) result ( str )
      class ( nombre_t ), intent ( in ) :: this
      character ( len=128 ) :: str
    end function rhyme_nombre_print
  end interface


  interface operator ( .u. )
    procedure rhyme_nombre_new
  end interface operator ( .u. )


  interface operator ( .unit. )
    procedure rhyme_nombre_new
  end interface operator ( .unit. )


  interface operator ( .to. )
    procedure rhyme_nombre_to
  end interface operator ( .to. )

  interface operator ( * )
    procedure rhyme_nombre_mul
    procedure rhyme_nombre_mul_rev
  end interface operator ( * )


  interface operator ( / )
    procedure rhyme_nombre_div
    procedure rhyme_nombre_div_rev
  end interface operator ( / )
end module rhyme_nombre
