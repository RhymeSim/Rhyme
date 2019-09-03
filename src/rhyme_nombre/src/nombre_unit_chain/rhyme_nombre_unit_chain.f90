module rhyme_nombre_unit_chain
  use rhyme_nombre_unit

  implicit none

  type nombre_unit_chain_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb = ''
    real ( kind=8 ) :: conv = 1d0
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1d0
    type ( nombre_unit_chain_t ), pointer :: next => null(), prev => null()
    type ( nombre_unit_t ), pointer :: head => null()
  end type nombre_unit_chain_t


  interface
    module function rhyme_nombre_unit_chain_new () result ( chain )
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_new

    module function rhyme_nombre_unit_chain_clone ( chain ) result ( clone )
      type ( nombre_unit_chain_t ), intent ( in ) :: chain
      type ( nombre_unit_chain_t ), pointer :: clone
    end function rhyme_nombre_unit_chain_clone

    module function rhyme_nombre_unit_chain_tail ( c ) result ( tail )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      type ( nombre_unit_t ), pointer :: tail
    end function rhyme_nombre_unit_chain_tail

    module function rhyme_nombre_unit_chain_mul_uu ( u1, u2 ) result ( chain )
      type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_uu

    module function rhyme_nombre_unit_chain_mul_cu ( c, u ) result ( chain )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_cu
  end interface

  interface rhyme_nombre_unit_chain_mul
    procedure rhyme_nombre_unit_chain_mul_uu
  end interface rhyme_nombre_unit_chain_mul

  interface operator ( * )
    procedure rhyme_nombre_unit_chain_mul_uu
    procedure rhyme_nombre_unit_chain_mul_cu
  end interface operator ( * )
end module rhyme_nombre_unit_chain
