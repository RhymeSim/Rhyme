module rhyme_nombre_base_unit_chain
  use rhyme_nombre_base_unit

  implicit none

  interface
    module function rhyme_nombre_base_unit_chain_clone ( u ) result ( clone )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: clone
    end function rhyme_nombre_base_unit_chain_clone

    module function rhyme_nombre_base_unit_chain_head ( u ) result ( head )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: head
    end function rhyme_nombre_base_unit_chain_head

    module function rhyme_nombre_base_unit_chain_tail ( u ) result ( tail )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: tail
    end function rhyme_nombre_base_unit_chain_tail
  end interface

  interface operator ( .clonechain. )
    module procedure rhyme_nombre_base_unit_chain_clone
  end interface operator ( .clonechain. )

  interface operator ( .head. )
    module procedure rhyme_nombre_base_unit_chain_head
  end interface operator ( .head. )

  interface operator ( .tail. )
    module procedure rhyme_nombre_base_unit_chain_tail
  end interface operator ( .tail. )
end module rhyme_nombre_base_unit_chain
