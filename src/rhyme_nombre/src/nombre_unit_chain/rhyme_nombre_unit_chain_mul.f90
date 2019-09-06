submodule ( rhyme_nombre_unit_chain ) mul_smod
contains
  module function rhyme_nombre_unit_chain_mul_iu ( i, u ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%conv = i
    chain%head => rhyme_nombre_unit_clone( u )

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_iu

  module function rhyme_nombre_unit_chain_mul_ru ( r, u ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%conv = real( r, kind=8 )
    chain%head => rhyme_nombre_unit_clone( u )

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_ru

  module function rhyme_nombre_unit_chain_mul_r8u ( r8, u ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%conv = r8
    chain%head => rhyme_nombre_unit_clone( u )

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_r8u

  module function rhyme_nombre_unit_chain_mul_uu ( u1, u2 ) result ( chain )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_new()
    chain%head => rhyme_nombre_unit_clone( u1 )
    chain%head%next => rhyme_nombre_unit_clone( u2 )
    chain%head%next%prev => chain%head

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_uu

  module function rhyme_nombre_unit_chain_mul_cu ( c, u ) result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_unit_chain_t ), pointer :: chain

    type ( nombre_unit_t ), pointer :: tail

    chain => rhyme_nombre_unit_chain_clone( c )
    tail => rhyme_nombre_unit_tail( chain%head )

    tail%next => rhyme_nombre_unit_clone( u**(1d0/chain%pow) )
    tail%next%prev => tail

    chain%dim = rhyme_nombre_unit_chain_get_dim( chain )
  end function rhyme_nombre_unit_chain_mul_cu

  module function rhyme_nombre_unit_chain_mul_cc ( c1, c2 ) result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c1, c2
    type ( nombre_unit_chain_t ), pointer :: chain

    type ( nombre_unit_chain_t ), pointer :: c1_ptr, c2_ptr
    type ( nombre_unit_chain_t ), pointer :: c1clone_ptr, c2clone_ptr
    type ( nombre_unit_chain_t ), pointer :: c1clone_tail, c2clone_head

    c1clone_ptr => null()
    c2clone_ptr => null()

    c1_ptr => rhyme_nombre_unit_chain_head( c1 )
    if ( associated( c1_ptr ) ) then
      c1clone_ptr => rhyme_nombre_unit_chain_clone( c1_ptr )

      do while ( associated( c1_ptr%next ) )
        c1clone_ptr%next => rhyme_nombre_unit_chain_clone( c1_ptr%next )
        c1clone_ptr%next%prev => c1clone_ptr

        c1_ptr => c1_ptr%next
        c1clone_ptr => c1clone_ptr%next
      end do
    end if

    c2_ptr => rhyme_nombre_unit_chain_head( c2 )
    if ( associated( c2_ptr ) ) then
      c2clone_ptr => rhyme_nombre_unit_chain_clone( c2_ptr )

      do while ( associated( c2_ptr%next ) )
        c2clone_ptr%next => rhyme_nombre_unit_chain_clone( c2_ptr%next )
        c2clone_ptr%next%prev => c2clone_ptr

        c2_ptr => c2_ptr%next
        c2clone_ptr => c2clone_ptr%next
      end do
    end if

    c1clone_tail => rhyme_nombre_unit_chain_tail( c1clone_ptr )
    c2clone_head => rhyme_nombre_unit_chain_tail( c2clone_ptr )

    c1clone_tail%next => c2clone_head
    c2clone_head%prev => c1clone_tail

    chain => rhyme_nombre_unit_chain_head( c2clone_head )
  end function rhyme_nombre_unit_chain_mul_cc

  module function rhyme_nombre_unit_chain_mul_ic ( i, c ) result ( chain )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_clone( c )
    chain%conv = chain%conv * i
  end function rhyme_nombre_unit_chain_mul_ic

  module function rhyme_nombre_unit_chain_mul_rc ( r, c ) result ( chain )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_clone( c )
    chain%conv = chain%conv * real( r, kind=8 )
  end function rhyme_nombre_unit_chain_mul_rc

  module function rhyme_nombre_unit_chain_mul_r8c ( r8, c ) result ( chain )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_clone( c )
    chain%conv = chain%conv * r8
  end function rhyme_nombre_unit_chain_mul_r8c

  module function rhyme_nombre_unit_chain_mul_pc ( p, c ) result ( chain )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: chain

    chain => rhyme_nombre_unit_chain_clone( c )
    chain%prefix = p
  end function rhyme_nombre_unit_chain_mul_pc
end submodule mul_smod
