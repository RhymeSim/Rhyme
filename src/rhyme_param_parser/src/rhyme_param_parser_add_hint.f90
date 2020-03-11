submodule(rhyme_param_parser) rhyme_param_parser_add_hint_submodule
contains
module function rhyme_param_parser_add_hint(term, hint) result(nterm)
   implicit none

   type(config_term_t), intent(in) :: term
   character(len=*), intent(in) :: hint

   type(config_term_t) :: nterm

   nterm%key = term%key
   nterm%location = term%location
   nterm%occurence = term%occurence
   nterm%hint = hint
end function rhyme_param_parser_add_hint
end submodule rhyme_param_parser_add_hint_submodule
