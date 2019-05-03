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
    procedure :: p => rhyme_nombre_unit_print
  end type nombre_unit_t

  interface
    module function rhyme_nombre_unit_pow ( u, pow ) result ( new_u )
      type ( nombre_unit_t ), intent ( in ), target :: u
      class (*), intent ( in ) :: pow
      type ( nombre_unit_t ), pointer :: new_u
    end function rhyme_nombre_unit_pow

    module function rhyme_nombre_unit_mul ( u, mul ) result ( new_u_tail )
      type ( nombre_unit_t ), intent ( in ), target :: u
      class (*), intent ( in ) :: mul
      type ( nombre_unit_t ), pointer :: new_u_tail
    end function rhyme_nombre_unit_mul

    module function rhyme_nombre_unit_prefix_mul ( p, u ) result ( new_u )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new_u
    end function rhyme_nombre_unit_prefix_mul

    module function rhyme_nombre_unit_div ( u1, u2 ) result ( u2_tail )
      type ( nombre_unit_t ), intent ( in ), target :: u1, u2
      type ( nombre_unit_t ), pointer :: u2_tail
    end function rhyme_nombre_unit_div

    module function rhyme_nombre_unit_print ( u ) result ( str )
      class ( nombre_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_unit_print

    module function rhyme_nombre_unit_clone ( u, hard ) result ( clone )
      type ( nombre_unit_t ), intent ( in ), target :: u
      logical, intent ( in ), optional :: hard
      type ( nombre_unit_t ), pointer :: clone
    end function rhyme_nombre_unit_clone

    module function rhyme_nombre_unit_head ( u ) result ( head )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: head
    end function rhyme_nombre_unit_head

    module function rhyme_nombre_unit_tail ( u ) result ( tail )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: tail
    end function rhyme_nombre_unit_tail
  end interface


  interface operator ( ** )
    procedure rhyme_nombre_unit_pow
  end interface operator ( ** )


  interface operator ( * )
    procedure rhyme_nombre_unit_mul
    procedure rhyme_nombre_unit_prefix_mul
  end interface operator ( * )


  interface operator ( / )
    procedure rhyme_nombre_unit_div
  end interface operator ( / )

contains

  function nombre_unit_get_conv ( u ) result ( conv )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u
    real ( kind=8 ) :: conv

    type ( nombre_unit_t ), pointer :: u_p

    u_p => u
    u_p => rhyme_nombre_unit_head( u_p )
    conv = ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow

    if ( abs( conv ) < epsilon(0.d0) ) conv = 1.d0

    do while ( associated( u_p%next ) )
      u_p => u_p%next
      conv = conv * ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow
    end do
  end function nombre_unit_get_conv
end module rhyme_nombre_unit
