submodule ( rhyme_param_parser ) rhyme_param_parser_new_term_submodule
contains
  pure module function rhyme_param_parser_new_term ( key, loc ) result ( term )
    implicit none

    character ( len=* ), intent ( in ) :: key
    integer, intent ( in ) :: loc

    type ( config_term_t ) :: term

    term%key = trim( key )
    term%location = loc
  end function rhyme_param_parser_new_term
end submodule rhyme_param_parser_new_term_submodule
