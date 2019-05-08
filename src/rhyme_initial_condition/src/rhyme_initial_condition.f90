module rhyme_initial_condition
  use rhyme_units
  use rhyme_samr
  use rhyme_chombo
  use rhyme_log

  implicit none

  type initial_condition_indices_t
    integer :: unset = -1
    integer :: simple = 1, snapshot = 2
    integer :: rhyme = 10, radamesh = 11
    character ( len=8 ), dimension(3) :: prob_domain_headers = [ &
      'hi_i    ', 'hi_j    ', 'hi_k    ' ]
    character ( len=8 ) :: boxes_headers(6) = [ &
      'lo_i    ', 'lo_j    ', 'lo_k    ', 'hi_i    ', 'hi_j    ', 'hi_k    ' ]
  end type initial_condition_indices_t

  type ( initial_condition_indices_t ), parameter :: icid = initial_condition_indices_t ()


  type initial_condition_t
    integer :: type = icid%unset
    integer :: snapshot_type = icid%unset
    integer :: nlevels = icid%unset
    integer :: base_grid(3) = icid%unset
    integer :: max_nboxes( 0:samrid%max_nlevels ) = 0
    character ( len=32 ) :: box_length_unit
    type ( nombre_t ) :: box_lengths(3)
    character ( len=1024 ) :: snapshot_path = ''
  end type initial_condition_t


  interface
    module subroutine rhyme_initial_condition_init ( ic, samr, units, logger )
      type ( initial_condition_t ), intent ( inout ) :: ic
      type ( samr_t ), intent ( inout ) :: samr
      type ( rhyme_units_t ), intent ( in ) :: units
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_initial_condition_init

    module subroutine rhyme_initial_condition_init_simple ( ic, samr, units, logger )
      type ( initial_condition_t ), intent ( in ) :: ic
      type ( samr_t ), intent ( inout ) :: samr
      type ( rhyme_units_t ), intent ( in ) :: units
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_initial_condition_init_simple

    module subroutine rhyme_initial_condition_load_snapshot ( ic, samr, logger )
      type ( initial_condition_t ), intent ( in ) :: ic
      type ( samr_t ), intent ( inout ) :: samr
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_initial_condition_load_snapshot

    module subroutine rhyme_initial_condition_load_headers ( ic, samr )
      type ( initial_condition_t ), intent ( in ) :: ic
      type ( samr_t ), intent ( inout ) :: samr
    end subroutine rhyme_initial_condition_load_headers

    module subroutine rhyme_initial_condition_load_rhyme ( ic, samr, logger )
      type ( initial_condition_t ), intent ( in ) :: ic
      type ( samr_t ), intent ( inout ) :: samr
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_initial_condition_load_rhyme
  end interface
end module rhyme_initial_condition
