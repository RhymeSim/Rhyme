module rhyme_nombre_derived_unit_chain
  use rhyme_nombre_derived_unit

  implicit none

  type nombre_derived_unit_chain_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1d0
    type ( nombre_derived_unit_t ) :: chain
    type ( nombre_derived_unit_chain_t ), pointer :: next => null(), prev => null()
  end type nombre_derived_unit_chain_t


  interface
    module function rhyme_nombre_derived_unit_chain_head ( chain ) result ( head )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: head
    end function rhyme_nombre_derived_unit_chain_head

    module function rhyme_nombre_derived_unit_chain_tail ( chain ) result ( tail )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: tail
    end function rhyme_nombre_derived_unit_chain_tail

    module function rhyme_nombre_derived_unit_chain_clone ( chain ) result ( clone )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: clone
    end function rhyme_nombre_derived_unit_chain_clone



    module function rhyme_nombre_derived_unit_chain_mul_dudu ( dunit1, dunit2 ) result ( chain )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit1, dunit2
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_chain_mul_dudu

    module function rhyme_nombre_derived_unit_chain_mul_duu ( c, u ) result ( chain )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: c
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_chain_mul_duu

    module function rhyme_nombre_derived_unit_chain_mul_udu ( u, c ) result ( chain )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), target, intent ( in ) :: c
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_chain_mul_udu
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_derived_unit_chain_mul_dudu
    module procedure rhyme_nombre_derived_unit_chain_mul_duu
    module procedure rhyme_nombre_derived_unit_chain_mul_udu
  end interface operator ( * )
end module rhyme_nombre_derived_unit_chain
