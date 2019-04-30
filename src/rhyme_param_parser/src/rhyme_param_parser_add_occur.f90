submodule ( rhyme_param_parser ) rhyme_param_parser_add_occur_submodue
contains
  module function rhyme_param_parser_add_occur ( term, occur ) result ( nterm )
    implicit none

    type ( config_term_t ), intent ( in ) :: term
    integer, intent ( in ) :: occur

    type ( config_term_t ) :: nterm

    nterm%key = term%key
    nterm%location = term%location
    nterm%occurence = occur
  end function rhyme_param_parser_add_occur
end submodule rhyme_param_parser_add_occur_submodue
