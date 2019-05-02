module rhyme_nombre_unit
  use rhyme_nombre_dimension
  use rhyme_nombre_prefix

  implicit none

  type nombre_unit_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb
    real ( kind=8 ) :: conv
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1.d0
    logical :: cloned = .false.
    type ( nombre_unit_t ), pointer :: next => null(), prev => null()
  contains
    procedure :: p => nombre_unit_print
  end type nombre_unit_t


  interface operator ( ** )
    procedure nombre_unit_int_pow
    procedure nombre_unit_real_pow
    procedure nombre_unit_real8_pow
  end interface operator ( ** )


  interface operator ( * )
    procedure nombre_unit_mul
    procedure nombre_unit_int_mul
    procedure nombre_unit_real_mul
    procedure nombre_unit_real8_mul
    procedure nombre_unit_prefix_mul
  end interface operator ( * )


  interface operator ( / )
    procedure nombre_unit_div
  end interface operator ( / )

contains

  function nombre_unit_print ( u ) result ( str )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    type ( nombre_unit_t ), pointer :: u_head

    u_head => u
    u_head => nombre_unit_head( u_head )
    str = ""

    do while ( associated( u_head ) )
      if ( abs( u_head%pow ) < epsilon(0.d0) ) then
        write( str, fmt="(A)" ) trim(str)//" "//trim(u_head%prefix%symb)//trim(u_head%symb)
      else
        if ( abs( int(u_head%pow) - u_head%pow ) < epsilon(0.d0) ) then
          write ( str, fmt="(A,I0)" ) trim(str)//" "//trim(u_head%prefix%symb)//trim(u_head%symb)//"^", int(u_head%pow)
        else
          write ( str, fmt="(A,F0.1)" ) trim(str)//" "//trim(u_head%prefix%symb)//trim(u_head%symb)//"^", u_head%pow
        end if
      end if

      u_head => u_head%next
    end do

    str = adjustl( str )
  end function nombre_unit_print


  function nombre_unit_hard_clone ( u ) result ( clone )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: clone

    type ( nombre_unit_t ), pointer :: u_ptr

    u_ptr => u
    u_ptr => nombre_unit_head( u_ptr )

    if ( associated( u_ptr ) ) allocate( clone )

    do while ( associated( u_ptr ) )

      clone%prefix = u_ptr%prefix
      clone%symb = u_ptr%symb
      clone%conv = u_ptr%conv
      clone%dim = u_ptr%dim
      clone%pow = u_ptr%pow
      clone%cloned = .true.

      if ( associated( u_ptr%next ) ) then
        allocate( clone%next )
        clone%next%prev => clone
        clone => clone%next
      end if

      u_ptr => u_ptr%next
    end do

    clone => nombre_unit_head( clone )
  end function nombre_unit_hard_clone


  function nombre_unit_soft_clone ( u ) result ( clone )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: clone

    if ( u%cloned ) then
      clone => u
    else
      allocate( clone )

      clone%prefix = u%prefix
      clone%symb = u%symb
      clone%conv = u%conv
      clone%dim = u%dim
      clone%pow = u%pow
      clone%cloned = .true.
      clone%next => u%next
      clone%prev => u%prev

      if ( associated( clone%prev ) ) clone%prev%next = clone
      if ( associated( clone%next ) ) clone%next%prev = clone
    end if
  end function nombre_unit_soft_clone


  function nombre_unit_head ( u ) result ( head )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: head

    head => u

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function nombre_unit_head


  function nombre_unit_tail ( u ) result ( tail )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: tail

    tail => u

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function nombre_unit_tail


  function nombre_unit_get_conv ( u ) result ( conv )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u
    real ( kind=8 ) :: conv

    type ( nombre_unit_t ), pointer :: u_p

    u_p => u
    u_p => nombre_unit_head( u_p )
    conv = ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow

    if ( abs( conv ) < epsilon(0.d0) ) conv = 1.d0

    do while ( associated( u_p%next ) )
      u_p => u_p%next
      conv = conv * ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow
    end do
  end function nombre_unit_get_conv


  function nombre_unit_int_pow ( u1, p ) result ( u )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1
    integer, intent ( in ) :: p

    type ( nombre_unit_t ), pointer :: u

    u => u1

    do while ( associated(u) )
      u => nombre_unit_soft_clone(u)
      u%pow = u%pow * real( p, kind=8 )

      if ( associated( u%prev ) ) then
        u => u%prev
      else
        exit
      end if
    end do

    u => nombre_unit_tail(u)
  end function nombre_unit_int_pow


  function nombre_unit_real_pow ( u1, p ) result ( u )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1
    real, intent ( in ) :: p

    type ( nombre_unit_t ), pointer :: u

    u => u1

    do while ( associated(u) )
      u => nombre_unit_soft_clone(u)
      u%pow = u%pow * p

      if ( associated( u%prev ) ) then
        u => u%prev
      else
        exit
      end if
    end do

    u => nombre_unit_tail(u)
  end function nombre_unit_real_pow


  function nombre_unit_real8_pow ( u1, p ) result ( u )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1
    real ( kind=8 ), intent ( in ) :: p

    type ( nombre_unit_t ), pointer :: u

    u => u1

    do while ( associated(u) )
      u => nombre_unit_soft_clone(u)
      u%pow = u%pow * p

      if ( associated( u%prev ) ) then
        u => u%prev
      else
        exit
      end if
    end do

    u => nombre_unit_tail(u)
  end function nombre_unit_real8_pow


  function nombre_unit_mul ( u1, u2 ) result ( u2_tail )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_unit_t ), pointer :: u2_tail, u2_head

    u2_head => nombre_unit_head( nombre_unit_soft_clone(u2) )

    u2_head%prev => nombre_unit_soft_clone(u1)

    u2_head%prev%next => u2_head

    u2_tail => nombre_unit_tail( u2_head )
  end function nombre_unit_mul


  function nombre_unit_real_mul ( r, u1 ) result ( u2 )
    implicit none

    real, intent ( in ) :: r
    type ( nombre_unit_t ), target, intent ( in ) :: u1

    type ( nombre_unit_t ), pointer :: u2

    u2 => nombre_unit_soft_clone(u1)

    u2%conv = u2%conv * real( r, kind=8 )
  end function nombre_unit_real_mul


  function nombre_unit_real8_mul ( r, u1 ) result ( u2 )
    implicit none

    real ( kind=8 ), intent ( in ) :: r
    type ( nombre_unit_t ), target, intent ( in ) :: u1

    type ( nombre_unit_t ), pointer :: u2

    u2 => nombre_unit_soft_clone(u1)

    u2%conv = u2%conv * r
  end function nombre_unit_real8_mul


  function nombre_unit_int_mul ( r, u1 ) result ( u2 )
    implicit none

    integer, intent ( in ) :: r
    type ( nombre_unit_t ), target, intent ( in ) :: u1

    type ( nombre_unit_t ), pointer :: u2

    u2 => nombre_unit_soft_clone(u1)

    u2%conv = u2%conv * real( r, kind=8 )
  end function nombre_unit_int_mul


  function nombre_unit_prefix_mul ( p, u1 ) result ( u2 )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_unit_t ), target, intent ( in ) :: u1
    type ( nombre_unit_t ), pointer :: u2

    integer :: i

    u2 => nombre_unit_soft_clone(u1)

    i = p%base_10 + u1%prefix%base_10

    u2%prefix = p * u2%prefix

    if ( u2%prefix%base_10 > 24 ) then
      u2%prefix = prfx_si(24)
      u2%conv = u2%conv * 10**( u2%prefix%base_10 - 24 )
    else if ( u2%prefix%base_10 < -24 ) then
      u2%prefix = prfx_si(-24)
      u2%conv = u2%conv * 1.d1**( u2%prefix%base_10 + 24 )
    else if ( ( u2%prefix%base_10 > 3 .or. u2%prefix%base_10 < -3 ) &
      .and. mod( u2%prefix%base_10, 3 ) /= 0 ) then
      u2%conv = u2%conv * 1.d1**( mod( u2%prefix%base_10, 3 ) )
      u2%prefix = prfx_si( ( u2%prefix%base_10 / 3 ) * 3 )
    end if
  end function nombre_unit_prefix_mul


  function nombre_unit_div ( u1, u2 ) result ( u2_tail )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_unit_t ), pointer :: u2_tail, u1_p, u2_p

    u1_p => nombre_unit_soft_clone(u1)
    u2_p => nombre_unit_soft_clone(u2)

    u1_p => nombre_unit_tail(u1_p)
    u2_p => nombre_unit_tail(u2_p)


    ! Updating powers
    do while ( associated( u2_p ) )
      u2_p%pow = -u2_p%pow

      if ( associated( u2_p%prev ) ) then
        u2_p => u2_p%prev
      else
        exit
      end if
    end do

    u2_p%prev => u1_p

    u2_p%prev%next => u2_p

    u2_tail => nombre_unit_tail( u2_p )
  end function nombre_unit_div
end module rhyme_nombre_unit
