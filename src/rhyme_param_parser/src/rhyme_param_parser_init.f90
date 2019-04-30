submodule ( rhyme_param_parser ) rhyme_param_parser_init_submodule
contains
  pure module subroutine rhyme_param_parser_init ( this, path, logger )
    implicit none

    class ( config_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: path
    type ( log_t ), intent ( inout ) :: logger

    this%path = trim( adjustl( path ) )
    this%logger = logger
  end subroutine rhyme_param_parser_init
end submodule rhyme_param_parser_init_submodule
