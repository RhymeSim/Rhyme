module rhyme_nombre
  use rhyme_nombre_units

  implicit none

  type nombre_t
    real ( kind=8 ) :: v
    type ( nombre_unit_t ), pointer :: u => null()
    contains
     procedure :: p => nombre_print
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

contains


  function nombre_print ( this ) result ( str )
    implicit none

    class ( nombre_t ), intent ( in ) :: this
    character ( len=128 ) :: str

    write (str, fmt="(E9.3,A,A,A)") this%v, " [ ", trim(this%u%p()), " ]"
  end function nombre_print




  function rhyme_nombre_div_real8_nombre ( r, n ) result ( n_new )
    implicit none

    real ( kind=8 ), intent ( in ) :: r
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( n%u, hard=.true. ) )

    n_new = nombre_t( r / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_real8_nombre


  function rhyme_nombre_div_nombre_real8 ( n, r ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real ( kind=8 ), intent ( in ) :: r

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( n%u, hard=.true. ) )

    n_new = nombre_t( n%v / r, u )
  end function rhyme_nombre_div_nombre_real8


  function rhyme_nombre_div_real_nombre ( r, n ) result ( n_new )
    implicit none

    real, intent ( in ) :: r
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( n%u, hard=.true. ) )

    n_new = nombre_t( real( r, kind=8 ) / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_real_nombre


  function rhyme_nombre_div_nombre_real ( n, r ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real, intent ( in ) :: r

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( n%u, hard=.true. ) )

    n_new = nombre_t( n%v / real( r, kind=8 ), u )
  end function rhyme_nombre_div_nombre_real


  function rhyme_nombre_div_int_nombre ( i, n ) result ( n_new )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( n%u, hard=.true. ) )

    n_new = nombre_t( real( i, kind=8 ) / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_int_nombre


end module rhyme_nombre
