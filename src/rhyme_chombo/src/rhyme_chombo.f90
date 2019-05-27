module rhyme_chombo
  use rhyme_hdf5_util
  use rhyme_samr
  use rhyme_log

  implicit none

  type rhyme_chombo_indices_t
    integer :: unset = h5id%unset
  end type rhyme_chombo_indices_t

  type ( rhyme_chombo_indices_t ), parameter :: chid = rhyme_chombo_indices_t ()

  type, extends ( rhyme_hdf5_util_t ) :: chombo_t
    logical :: is_opened = .false.
    integer :: num_levels = chid%unset
    integer :: num_components = chid%unset
    integer :: iteration = chid%unset
    integer ( hid_t ) :: chombo_global_id = chid%unset
    integer ( hid_t ) :: level_ids(0:23) = chid%unset
    character ( len=1024 ) :: prefix = ""
    character ( len=1024 ) :: nickname = ""
  contains
    procedure :: init => rhyme_chombo_init
    procedure :: create_chombo => rhyme_chombo_create_chombo
    procedure :: write_headers => rhyme_chombo_write_headers
    procedure :: write_samr => rhyme_chombo_write_samr
    procedure :: write_level_data => rhyme_chombo_write_level_data
    procedure :: filename_generator => rhyme_chombo_filename_generator
  end type chombo_t

  interface
    module subroutine rhyme_chombo_init ( this, logger )
      class ( chombo_t ), intent ( in ) :: this
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_chombo_init

    module subroutine rhyme_chombo_filename_generator ( this, filename )
      class ( chombo_t ), intent ( in ) :: this
      character ( len=1024 ), intent ( out ) :: filename
    end subroutine rhyme_chombo_filename_generator

    module subroutine rhyme_chombo_create_chombo ( this )
      class ( chombo_t ), intent (inout) :: this
    end subroutine rhyme_chombo_create_chombo

    module subroutine rhyme_chombo_write_headers ( this, samr )
      class ( chombo_t ), intent (inout) :: this
      type ( samr_t ), intent ( in ) :: samr
    end subroutine rhyme_chombo_write_headers

    module subroutine rhyme_chombo_write_level_data ( this, level )
      class ( chombo_t ), intent ( inout ) :: this
      type ( samr_level_t ), intent ( in ) :: level
    end subroutine rhyme_chombo_write_level_data

    module subroutine rhyme_chombo_write_samr ( this, samr )
      class ( chombo_t ), intent (inout) :: this
      type ( samr_t ), intent (in) :: samr
    end subroutine rhyme_chombo_write_samr
  end interface
end module rhyme_chombo
