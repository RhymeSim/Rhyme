module rhyme_nombre_derived_unit_chain
  use rhyme_nombre_derived_unit

  implicit none

  type nombre_derived_unit_chain_t
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


    module function rhyme_nombre_derived_unit_chain_mul_cc ( dunit1, dunit2 ) result ( chain )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit1, dunit2
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_chain_mul_cc
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_derived_unit_chain_mul_cc
  end interface operator ( * )
end module rhyme_nombre_derived_unit_chain
