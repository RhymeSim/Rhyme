module rhyme_chombo_factory
  use rhyme_chombo

  implicit none

  type rhyme_chombo_factory_t
    ! default variables
    logical :: is_opened = .false.
    integer :: num_levels = chid%unset
    integer :: num_components = chid%unset
    integer :: iteration = chid%unset
    integer ( hid_t ) :: chombo_global_id = chid%unset
    integer ( hid_t ) :: level_ids(0:23) = chid%unset
    character ( len=1024 ) :: prefix = ""
    character ( len=1024 ) :: nickname = ""
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_chombo_factory_init
    procedure :: generate => rhyme_chombo_factory_generate
  end type rhyme_chombo_factory_t

  type ( rhyme_chombo_factory_t ) :: ch_factory = rhyme_chombo_factory_t()

contains

  subroutine rhyme_chombo_factory_init ( this )
    implicit none

    class ( rhyme_chombo_factory_t ), intent ( inout ) :: this

    this%is_opened = .false.
    this%num_levels = chid%unset
    this%num_components = chid%unset
    this%iteration = chid%unset
    this%chombo_global_id = chid%unset
    this%level_ids = chid%unset
    this%prefix = ""
    this%nickname = ""

    this%initialized = .true.
  end subroutine rhyme_chombo_factory_init


  function rhyme_chombo_factory_generate ( this ) result ( ch )
    implicit none

    class ( rhyme_chombo_factory_t ), intent ( inout ) :: this
    type ( chombo_t ) :: ch

    if ( .not. this%initialized ) call this%init

    ch%is_opened = this%is_opened
    ch%num_levels = this%num_levels
    ch%num_components = this%num_components
    ch%iteration = this%iteration
    ch%chombo_global_id = this%chombo_global_id
    ch%level_ids = this%level_ids
    ch%prefix = this%prefix
    ch%nickname = this%nickname

  end function rhyme_chombo_factory_generate
end module rhyme_chombo_factory
