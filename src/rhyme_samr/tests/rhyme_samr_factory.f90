module rhyme_samr_factory
  use rhyme_samr
  use rhyme_hydro_base

  implicit none

  type rhyme_samr_factory_t
    ! Default values
    integer :: nlevels = 3
    integer :: max_nboxes( 0:samrid%max_nlevels ) = 0
    integer :: init_nboxes( 0:samrid%max_nlevels ) = 0
#if NDIM == 1
    integer :: base_grid( NDIM ) = [ 16 ]
    integer :: ghost_cells( NDIM ) = [ 2 ]
    real ( kind=8 ) :: box_lengths( NDIM ) = [ 1.d0 ]
#elif NDIM == 2
    integer :: base_grid( NDIM ) = [ 16, 8 ]
    integer :: ghost_cells( NDIM ) = [ 2, 2 ]
    real ( kind=8 ) :: box_lengths( NDIM ) = [ 1.d0, .5d0 ]
#elif NDIM == 3
    integer :: base_grid( NDIM ) = [ 16, 8, 4 ]
    integer :: ghost_cells( NDIM ) = [ 2, 2, 2 ]
    real ( kind=8 ) :: box_lengths( NDIM ) = [ 1.d0, .5d0, .25d0 ]
#endif
  contains
    procedure :: init => rhyme_samr_factory_init
    procedure :: generate => rhyme_samr_factory_generate
    procedure :: generate_with => rhyme_samr_factory_generate_with
  end type rhyme_samr_factory_t

  type ( rhyme_samr_factory_t ) :: samr_factory = rhyme_samr_factory_t()

contains

  subroutine rhyme_samr_factory_init ( this, empty )
    implicit none

    class ( rhyme_samr_factory_t ), intent ( inout ) :: this
    logical, intent ( in ), optional :: empty

    this%max_nboxes = 0
    this%max_nboxes( 0:2 ) = [ 1, 3, 9 ]
    this%init_nboxes = 0
    if ( present( empty ) .and. .not. empty ) then
      this%init_nboxes( 0:2 ) = [ 1, 2, 4 ]
    end if
  end subroutine rhyme_samr_factory_init


  function rhyme_samr_factory_generate ( this, physical, empty ) result ( samr )
    implicit none

    class ( rhyme_samr_factory_t ), intent ( inout ) :: this
    logical, intent ( in ), optional :: physical, empty

    type ( samr_t ) :: samr

    logical :: phys, emp
    integer :: l

    if ( present( empty ) ) then
      emp = empty
    else
      emp = .false.
    end if

    call this%init( empty=emp )

    if ( present( physical ) ) then
      phys = physical
    else
      phys = .false.
    end if

    samr%nlevels = this%nlevels
    samr%base_grid = this%base_grid
    samr%ghost_cells = this%ghost_cells
    samr%box_lengths = this%box_lengths
    samr%max_nboxes = this%max_nboxes

    samr%levels%level = [ ( l, l=0, 23 ) ]
    samr%levels%max_nboxes = this%max_nboxes
    samr%levels%nboxes = this%init_nboxes

    call rhyme_samr_factory_filling( samr, this%init_nboxes, phys )
  end function rhyme_samr_factory_generate


  function rhyme_samr_factory_generate_with ( this, nlevels, base_grid, &
    ghost_cells, max_nboxes, init_nboxes, box_lengths, physical ) &
    result ( samr )
    implicit none

    class ( rhyme_samr_factory_t ), intent ( inout ) :: this
    integer, intent ( in ) :: nlevels, base_grid( NDIM ), ghost_cells( NDIM )
    integer, intent ( in ) :: max_nboxes( 0:samrid%max_nlevels )
    integer, intent ( in ) :: init_nboxes( 0:samrid%max_nlevels )
    real ( kind=8 ), intent ( in ) :: box_lengths( NDIM )
    logical, intent ( in ), optional :: physical

    type ( samr_t ) :: samr

    integer :: l
    logical :: phys

    ! Just to suppress the unused warning
    call this%init

    if ( present( physical ) ) then
      phys = physical
    else
      phys = .false.
    end if

    samr%nlevels = nlevels
    samr%base_grid = base_grid
    samr%ghost_cells = ghost_cells
    samr%box_lengths = box_lengths
    samr%max_nboxes = max_nboxes

    samr%levels%level = [ ( l, l=0, 23 ) ]
    samr%levels%max_nboxes = max_nboxes
    samr%levels%nboxes = init_nboxes

    call rhyme_samr_factory_filling( samr, init_nboxes, phys )
  end function rhyme_samr_factory_generate_with


  subroutine rhyme_samr_factory_filling ( samr, init_nboxes, physical )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    integer, intent ( in ) :: init_nboxes( 0:samrid%max_nlevels )
    logical, intent ( in ) :: physical

    real ( kind=8 ) :: val, state( NCMP )
    integer :: l, b, i, uid
#if NDIM > 1
    integer :: j
#endif
#if NDIM > 2
    integer :: k
#endif
    integer :: lb( NDIM ), ub( NDIM ), rand_len, box_dims( NDIM )

    if ( physical ) then
      rand_len = NCMP
      call random_seed( size = rand_len )
    end if

    do l = 0, samr%nlevels - 1
      allocate ( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      samr%levels(l)%iteration = 0
      samr%levels(l)%dx = samr%box_lengths / samr%base_grid / 2.d0**l
      samr%levels(l)%dt = 0.d0
      samr%levels(l)%t = 0.d0

      box_dims = floor( samr%base_grid / real( init_nboxes(l) ) )
      box_dims = merge( box_dims, 1, box_dims > 1 )

      do b = 1, samr%levels(l)%nboxes
        samr%levels(l)%boxes(b)%level = l
        samr%levels(l)%boxes(b)%number = b

        lb = -samr%ghost_cells + 1
        ub = box_dims + samr%ghost_cells

        samr%levels(l)%boxes(b)%dims = box_dims

#if NDIM == 1
        allocate( samr%levels(l)%boxes(b)%flags( lb(1):ub(1) ) )
        allocate( samr%levels(l)%boxes(b)%cells( lb(1):ub(1), NCMP ) )
#elif NDIM == 2
        allocate( samr%levels(l)%boxes(b)%flags( lb(1):ub(1), lb(2):ub(2) ) )
        allocate( samr%levels(l)%boxes(b)%cells( lb(1):ub(1), lb(2):ub(2), NCMP ) )
#elif NDIM == 3
        allocate( samr%levels(l)%boxes(b)%flags( lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) ) )
        allocate( samr%levels(l)%boxes(b)%cells( lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), NCMP ) )
#endif

        samr%levels(l)%boxes(b)%left_edge = (b - 1) * box_dims + 1
        samr%levels(l)%boxes(b)%right_edge = b * box_dims

#if NDIM > 2
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
#endif
#if NDIM > 1
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
#endif
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              if ( physical ) then
                state = gen_state()
#if NDIM == 1
                samr%levels(l)%boxes(b)%flags(i) = i
                samr%levels(l)%boxes(b)%cells(i, :) = state
#elif NDIM == 2
                samr%levels(l)%boxes(b)%flags(i, j) = j * box_dims(1) + i
                samr%levels(l)%boxes(b)%cells(i, j, :) = state
#elif NDIM == 3
                samr%levels(l)%boxes(b)%flags(i, j, k) = k * box_dims(2) * box_dims(1) + j * box_dims(1) + i
                samr%levels(l)%boxes(b)%cells(i, j, k,:) = state
#endif
              else
#if NDIM == 1
                val = l * 1d1 + b * 1d0 + i * 1d-2
                samr%levels(l)%boxes(b)%flags(i) = int ( val * 1e3 )
                do uid = 1, NCMP
                  samr%levels(l)%boxes(b)%cells(i, uid) = val + uid * 1d-7
                end do
#elif NDIM == 2
                val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4
                samr%levels(l)%boxes(b)%flags(i, j) = int ( val * 1e3 )
                do uid = 1, NCMP
                  samr%levels(l)%boxes(b)%cells(i, j, uid) = val + uid * 1d-7
                end do
#elif NDIM == 3
                val = l * 1d1 + b * 1d0 + i * 1d-2 + j * 1d-4 + k * 1d-6
                samr%levels(l)%boxes(b)%flags(i, j, k) = int ( val * 1e3 )
                do uid = 1, NCMP
                  samr%levels(l)%boxes(b)%cells(i, j, k, uid) = val + uid * 1d-7
                end do
#endif

              end if
            end do
#if NDIM > 1
          end do
#endif
#if NDIM > 2
        end do
#endif

      end do
    end do

    samr%initialized = .true.
  contains
    function gen_state () result ( u )
      implicit none

      real ( kind=8 ) :: u( NCMP )

      real ( kind=8 ) :: r( NCMP )
      integer :: i, n_hydro_vars

      n_hydro_vars = 1 + NDIM + 1

      call random_number( r )

      u(1) = r(1)

      do i = 2, NDIM + 1
        u(i) = r(1) * ( r(i) - .5d0 )
      end do

      u(n_hydro_vars) = .5d0 * sum( u( 2:NDIM+1 )**2 ) / r(1) + r(i+1) / ( 5.d0 / 3.d0 - 1 )

      do i = n_hydro_vars + 1, NCMP
        ! TODO: set temperature if it's available
        u(i) = 0.d0
      end do
    end function gen_state
  end subroutine rhyme_samr_factory_filling
end module rhyme_samr_factory
