submodule ( rhyme_nombre_unit_chain ) new_smod
contains
  module function rhyme_nombre_unit_chain_new () result ( chain )
    implicit none

    type ( nombre_unit_chain_t ), pointer :: chain

    allocate( chain )

    chain%prefix = null_prefix
    chain%symb = ''
    chain%conv = 1d0
    chain%dim = dimid%null
    chain%pow = 1d0
  end function rhyme_nombre_unit_chain_new
end submodule new_smod
