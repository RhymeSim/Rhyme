module rhyme_nombre
  use rhyme_nombre_units

  implicit none

  type nombre_t
    real ( kind=8 ) :: v
    type ( nombre_unit_t ), pointer :: u => null()
    contains
     procedure :: p => nombre_print
  end type nombre_t


  interface operator ( .u. )
    procedure rhyme_nombre_new_real8
    procedure rhyme_nombre_new_real
    procedure rhyme_nombre_new_int
  end interface operator ( .u. )


  interface operator ( .unit. )
    procedure rhyme_nombre_new_real8
    procedure rhyme_nombre_new_real
    procedure rhyme_nombre_new_int
  end interface operator ( .unit. )


  interface operator ( .to. )
    procedure rhyme_nombre_to
  end interface operator ( .to. )

  interface operator ( * )
    procedure rhyme_nombre_mul_int
    procedure rhyme_nombre_mul_int_rev
    procedure rhyme_nombre_mul_real
    procedure rhyme_nombre_mul_real_rev
    procedure rhyme_nombre_mul_real8
    procedure rhyme_nombre_mul_real8_rev
  end interface operator ( * )


  interface operator ( / )
    procedure rhyme_nombre_div_nombre_nombre
    procedure rhyme_nombre_div_real8_nombre
    procedure rhyme_nombre_div_nombre_real8
    procedure rhyme_nombre_div_real_nombre
    procedure rhyme_nombre_div_nombre_real
    procedure rhyme_nombre_div_int_nombre
    procedure rhyme_nombre_div_nombre_int
  end interface operator ( / )

contains

  function rhyme_nombre_new_real8 ( v, u ) result( n )
    implicit none

    real ( kind=8 ), intent(in) :: v
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_t ) :: n

    type ( nombre_unit_t ), pointer :: u_ptr

    u_ptr => u

    n = nombre_t( v, nombre_unit_head( u_ptr ) )
  end function rhyme_nombre_new_real8


  function rhyme_nombre_new_real ( v, u ) result ( n )
    implicit none

    real, intent ( in ) :: v
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_t ) :: n

    type ( nombre_unit_t ), pointer :: u_ptr

    u_ptr => u

    n = nombre_t( real( v, kind=8 ), nombre_unit_head( u_ptr ) )
  end function rhyme_nombre_new_real


  function rhyme_nombre_new_int ( v, u ) result ( n )
    implicit none

    integer, intent ( in ) :: v
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_t ) :: n

    type ( nombre_unit_t ), pointer :: u_ptr

    u_ptr => u

    n = nombre_t( real( v, kind=8 ), nombre_unit_head( u_ptr ) )
  end function rhyme_nombre_new_int


  function rhyme_nombre_to ( n, u_new ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_unit_t ), pointer, intent ( in ) :: u_new
    type ( nombre_t ) :: n_new

    real ( kind=8 ) :: co, cn

    co = nombre_unit_get_conv( n%u )
    cn = nombre_unit_get_conv( u_new )

    n_new%v = n%v * co / cn

    n_new%u => u_new
  end function rhyme_nombre_to


  function nombre_print ( this ) result ( str )
    implicit none

    class ( nombre_t ), intent ( in ) :: this
    character ( len=128 ) :: str

    write (str, fmt="(E9.3,A,A,A)") this%v, " [ ", trim(this%u%p()), " ]"
  end function nombre_print


  function rhyme_nombre_mul_int ( i, n ) result ( n_new )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone( n%u )

    n_new = nombre_t( real( i, kind=8 ) * n%v, u )
  end function rhyme_nombre_mul_int


  function rhyme_nombre_mul_int_rev ( n, i ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    integer, intent ( in ) :: i
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone( n%u )

    n_new = nombre_t( real( i, kind=8 ) * n%v, u )
  end function rhyme_nombre_mul_int_rev


  function rhyme_nombre_mul_real ( r, n ) result ( n_new )
    implicit none

    real, intent ( in ) :: r
    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone(n%u)

    n_new = nombre_t( real( r, kind=8 ) * n%v, u )
  end function rhyme_nombre_mul_real


  function rhyme_nombre_mul_real_rev ( n, r ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real, intent ( in ) :: r
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone( n%u )

    n_new = nombre_t( real( r, kind=8 ) * n%v, u )
  end function rhyme_nombre_mul_real_rev


  function rhyme_nombre_mul_real8 ( r8, n ) result ( n_new )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone( n%u )

    n_new = nombre_t( r8 * n%v, u )
  end function rhyme_nombre_mul_real8


  function rhyme_nombre_mul_real8_rev ( n, r8 ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_hard_clone( n%u )

    n_new = nombre_t( r8 * n%v, u )
  end function rhyme_nombre_mul_real8_rev


  function rhyme_nombre_div_nombre_nombre ( n1, n2 ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n1, n2
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u1, u2

    u1 => nombre_unit_hard_clone( n1%u )
    u2 => nombre_unit_hard_clone( n2%u )

    n_new = nombre_t( n1%v / n2%v, u1 / u2 )
  end function rhyme_nombre_div_nombre_nombre


  function rhyme_nombre_div_real8_nombre ( r, n ) result ( n_new )
    implicit none

    real ( kind=8 ), intent ( in ) :: r
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( r / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_real8_nombre


  function rhyme_nombre_div_nombre_real8 ( n, r ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real ( kind=8 ), intent ( in ) :: r

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( n%v / r, u )
  end function rhyme_nombre_div_nombre_real8


  function rhyme_nombre_div_real_nombre ( r, n ) result ( n_new )
    implicit none

    real, intent ( in ) :: r
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( real( r, kind=8 ) / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_real_nombre


  function rhyme_nombre_div_nombre_real ( n, r ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real, intent ( in ) :: r

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( n%v / real( r, kind=8 ), u )
  end function rhyme_nombre_div_nombre_real


  function rhyme_nombre_div_int_nombre ( i, n ) result ( n_new )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_t ), intent ( in ) :: n

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( real( i, kind=8 ) / n%v, u**(-1.d0) )
  end function rhyme_nombre_div_int_nombre


  function rhyme_nombre_div_nombre_int ( n, i ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    integer, intent ( in ) :: i

    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => nombre_unit_tail( nombre_unit_hard_clone( n%u ) )

    n_new = nombre_t( n%v / real( i, kind=8 ), u )
  end function rhyme_nombre_div_nombre_int
end module rhyme_nombre
