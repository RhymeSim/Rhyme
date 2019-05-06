module rhyme_param_parser
  use rhyme_log
  use rhyme_units
  use rhyme_initial_condition
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_drawing
  use rhyme_irs
  use rhyme_slope_limiter
  use rhyme_muscl_hancock
  use rhyme_chombo

  implicit none

  type config_t
    character ( len=1024 ) :: path
    type ( log_t ) :: logger
  contains
    procedure :: init => rhyme_param_parser_init
    procedure :: occur => rhyme_param_parser_occurences
    procedure :: read => rhyme_param_parser_read
    procedure :: read_array => rhyme_param_parser_read_array
  end type config_t


  type config_switch_t
    integer :: len = 0
    character ( len=32 ) :: keys(32) = ''
    integer :: values(32)
  contains
    procedure :: add => rhyme_param_parser_add_switch
  end type config_switch_t


  type config_term_t
    character ( len=32 ) :: key
    integer :: location = 1
    integer :: occurence = 1
    character ( len=1024 ) :: hint = ''
  end type config_term_t


  interface
    module subroutine load_params ( param_file, logger, units, ic, bc, cfl, ig, &
      draw, irs, sl, mh, chombo )
      character (len=1024), intent ( in ) :: param_file
      type ( log_t ), intent ( inout ) :: logger
      type ( rhyme_units_t ), intent ( inout ) :: units
      type ( initial_condition_t ), intent ( inout ) :: ic
      type ( samr_bc_t ), intent ( inout ) :: bc
      type ( cfl_t ), intent ( inout ) :: cfl
      type ( ideal_gas_t ), intent ( inout ) :: ig
      type ( drawing_t ), intent ( inout ) :: draw
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( inout ) :: sl
      type ( muscl_hancock_t ), intent ( inout ) :: mh
      type ( chombo_t ), intent ( inout ) :: chombo
    end subroutine load_params

    pure module subroutine rhyme_param_parser_init ( this, path, logger )
      class ( config_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: path
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_param_parser_init

    module subroutine rhyme_param_parser_read ( this, term, var, switch )
      class ( config_t ), intent ( inout ) :: this
      type ( config_term_t ), intent ( in ) :: term
      class (*), intent ( inout ) :: var
      type ( config_switch_t ), optional, intent ( in ) :: switch
    end subroutine rhyme_param_parser_read

    module subroutine rhyme_param_parser_read_array ( this, term, var, switch )
      class ( config_t ), intent ( inout ) :: this
      type ( config_term_t ), intent ( in ) :: term
      class (*), intent ( inout ) :: var(:)
      type ( config_switch_t ), optional, intent ( in ) :: switch
    end subroutine rhyme_param_parser_read_array

    pure module subroutine rhyme_param_parser_add_switch ( this, key, val )
      class ( config_switch_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: key
      integer, intent ( in ) :: val
    end subroutine rhyme_param_parser_add_switch

    pure module function rhyme_param_parser_new_term ( key, loc ) result ( term )
      character ( len=* ), intent ( in ) :: key
      integer, intent ( in ) :: loc
      type ( config_term_t ) :: term
    end function rhyme_param_parser_new_term

    module function rhyme_param_parser_occurences ( this, key ) result ( occur )
      class ( config_t ), intent ( in ) :: this
      character ( len=* ), intent ( in ) :: key
      integer :: occur
    end function rhyme_param_parser_occurences

    module function rhyme_param_parser_add_occur ( term, occur ) result ( nterm )
      type ( config_term_t ), intent ( in ) :: term
      integer, intent ( in ) :: occur
      type ( config_term_t ) :: nterm
    end function rhyme_param_parser_add_occur

    module function rhyme_param_parser_add_hint ( term, hint ) result ( nterm )
      type ( config_term_t ), intent ( in ) :: term
      character ( len=* ), intent( in ) :: hint
      type ( config_term_t ) :: nterm
    end function rhyme_param_parser_add_hint
  end interface

  interface operator ( .at. )
    procedure rhyme_param_parser_new_term
  end interface operator ( .at. )

  interface operator ( .occur. )
    procedure rhyme_param_parser_add_occur
  end interface operator ( .occur. )

  interface operator ( .hint. )
    procedure rhyme_param_parser_add_hint
  end interface operator ( .hint. )
end module rhyme_param_parser
