module rhyme_param_parser
   ! TODO: replace it with a JSON object reader
   use rhyme_chemistry
   use rhyme_units
   use rhyme_initial_condition
   use rhyme_samr_bc
   use rhyme_cfl
   use rhyme_thermo_base
   use rhyme_uv_background
   use rhyme_ionisation_equilibrium
   use rhyme_drawing
   use rhyme_irs
   use rhyme_slope_limiter
   use rhyme_muscl_hancock
   use rhyme_chombo
   use rhyme_report
   use rhyme_sanity_check
   use rhyme_logger

   implicit none

   type config_t
      character(len=1024) :: path
   contains
      procedure :: init => rhyme_param_parser_init
      procedure :: occur => rhyme_param_parser_occurences
      procedure :: read_single => rhyme_param_parser_read_single
      procedure :: read_array => rhyme_param_parser_read_array
      generic :: read => read_single, read_array
   end type config_t

   type config_switch_t
      integer :: len = 0
      character(len=32) :: types(32) = ''
      character(len=32) :: keys(32) = ''
      integer :: int_values(32) = -123456
      logical :: log_values(32) = .false.
   contains
      procedure :: add => rhyme_param_parser_add_switch
   end type config_switch_t

   type config_term_t
      character(len=128) :: key
      integer :: location = 1
      integer :: occurence = 1
      character(len=1024) :: hint = ''
   end type config_term_t

   interface
      module subroutine load_params( &
         param_file, chemistry, units, ic, bc, cfl, thermo, uvb, &
         ie, draw, irs, sl, mh, chombo, report, sc, logger)
         character(len=1024), intent(in) :: param_file
         type(chemistry_t), intent(inout) :: chemistry
         type(units_t), intent(inout) :: units
         type(initial_condition_t), intent(inout) :: ic
         type(samr_bc_t), intent(inout) :: bc
         type(cfl_t), intent(inout) :: cfl
         type(thermo_base_t), intent(inout) :: thermo
         type(uv_background_t), intent(inout) :: uvb
         type(ionisation_equilibrium_t), intent(inout) :: ie
         type(drawing_t), intent(inout) :: draw
         type(irs_t), intent(inout) :: irs
         type(slope_limiter_t), intent(inout) :: sl
         type(muscl_hancock_t), intent(inout) :: mh
         type(chombo_t), intent(inout) :: chombo
         type(report_t), intent(inout) :: report
         type(sanity_check_t), intent(inout) :: sc
         type(logger_t), intent(inout) :: logger
      end subroutine load_params

      module subroutine rhyme_param_parser_read_single(this, term, var, logger, switch)
         class(config_t), intent(inout) :: this
         type(config_term_t), intent(in) :: term
         class(*), intent(inout) :: var
         type(logger_t), intent(inout) :: logger
         type(config_switch_t), optional, intent(in) :: switch
      end subroutine rhyme_param_parser_read_single

      module subroutine rhyme_param_parser_read_array(this, term, var, logger, switch)
         class(config_t), intent(inout) :: this
         type(config_term_t), intent(in) :: term
         class(*), intent(inout) :: var(:)
         type(logger_t), intent(inout) :: logger
         type(config_switch_t), optional, intent(in) :: switch
      end subroutine rhyme_param_parser_read_array

      module subroutine rhyme_param_parser_add_switch(this, key, val)
         class(config_switch_t), intent(inout) :: this
         character(len=*), intent(in) :: key
         class(*), intent(in) :: val
      end subroutine rhyme_param_parser_add_switch

      pure module function rhyme_param_parser_new_term(key, loc) result(term)
         character(len=*), intent(in) :: key
         integer, intent(in) :: loc
         type(config_term_t) :: term
      end function rhyme_param_parser_new_term

      module function rhyme_param_parser_occurences(this, key) result(occur)
         class(config_t), intent(in) :: this
         character(len=*), intent(in) :: key
         integer :: occur
      end function rhyme_param_parser_occurences

      module function rhyme_param_parser_add_occur(term, occur) result(nterm)
         type(config_term_t), intent(in) :: term
         integer, intent(in) :: occur
         type(config_term_t) :: nterm
      end function rhyme_param_parser_add_occur

      module function rhyme_param_parser_add_hint(term, hint) result(nterm)
         type(config_term_t), intent(in) :: term
         character(len=*), intent(in) :: hint
         type(config_term_t) :: nterm
      end function rhyme_param_parser_add_hint
   end interface

   interface operator(.at.)
      procedure rhyme_param_parser_new_term
   end interface operator(.at.)

   interface operator(.occur.)
      procedure rhyme_param_parser_add_occur
   end interface operator(.occur.)

   interface operator(.hint.)
      procedure rhyme_param_parser_add_hint
   end interface operator(.hint.)

contains
   pure module subroutine rhyme_param_parser_init(this, path)
      implicit none

      class(config_t), intent(inout) :: this
      character(len=*), intent(in) :: path

      this%path = trim(adjustl(path))
   end subroutine rhyme_param_parser_init

end module rhyme_param_parser
