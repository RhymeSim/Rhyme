module rhyme_slope_limiter_factory
  use rhyme_slope_limiter

  implicit none

  type rhyme_slope_limiter_factory_t
    ! default variables
    real ( kind=8 ) :: w = 0.d0
    integer :: type = slid%minmod
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_slope_limiter_factory_init
    procedure :: generate => rhyme_slope_limiter_factory_generate
  end type rhyme_slope_limiter_factory_t

  type ( rhyme_slope_limiter_factory_t ) :: sl_factory = rhyme_slope_limiter_factory_t()

contains

  subroutine rhyme_slope_limiter_factory_init ( this )
    implicit none

    class ( rhyme_slope_limiter_factory_t ), intent ( inout ) :: this

    this%w = 0.d0
    this%type = slid%minmod

    this%initialized = .true.
  end subroutine rhyme_slope_limiter_factory_init


  function rhyme_slope_limiter_factory_generate ( this, sl_type ) result ( sl )
    implicit none

    class ( rhyme_slope_limiter_factory_t ), intent ( inout ) :: this
    integer, intent ( in ), optional :: sl_type

    type ( slope_limiter_t ) :: sl

    if ( .not. this%initialized ) call this%init

    sl%w = this%w

    if ( present( sl_type ) ) then
      sl%type = sl_type
    else
      sl%type = this%type
    end if
  end function rhyme_slope_limiter_factory_generate
end module rhyme_slope_limiter_factory
