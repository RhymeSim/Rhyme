submodule ( rhyme_samr ) rhyme_samr_init_box_submodule
contains
  module subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
    ! NB: This subroutine allocates a box at a given location and increments
    !     the number of boxes on that level. Make sure all the boxes between
    !     box 1 to box b in the level are allocated!
    !     Also the subroutine does not check if the box has already been
    !     allocated or not, very unsafe, I know ;)
    class ( samr_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b, dims( NDIM )
    integer, intent ( in ) :: ledges( NDIM ), redges( NDIM )

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J , lb(2):ub(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J , lb(2):ub(2)
#define RANGE_K , lb(3):ub(3)
#endif

    integer :: lb( NDIM ), ub( NDIM )

    this%levels(l)%boxes(b)%level = l
    this%levels(l)%boxes(b)%number = b

    lb = -this%ghost_cells + 1
    ub = dims + this%ghost_cells

    allocate( this%levels(l)%boxes(b)%flags( lb(1):ub(1) RANGE_J RANGE_K ) )
    allocate( this%levels(l)%boxes(b)%cells( lb(1):ub(1) RANGE_J RANGE_K, NCMP ) )

    this%levels(l)%nboxes = this%levels(l)%nboxes + 1

    this%levels(l)%boxes(b)%dims = dims
    this%levels(l)%boxes(b)%left_edge = ledges
    this%levels(l)%boxes(b)%right_edge = redges
  end subroutine rhyme_samr_init_box
end submodule rhyme_samr_init_box_submodule
