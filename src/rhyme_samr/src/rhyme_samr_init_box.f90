submodule ( rhyme_samr ) rhyme_samr_init_box_submodule
contains
  module subroutine rhyme_samr_init_box ( this, l, b, dims, ledges, redges )
    ! NB: This subroutine allocates a box at a given location and increments
    !     the number of boxes on that level. Make sure all the boxes between
    !     box 1 to box b in the level are allocated!
    !     Also the subroutine does not check if the box has already been
    !     allocated or not, very unsafe, I know ;)
    class ( samr_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b, dims(3)
    integer, intent ( in ) :: ledges(3), redges(3)

    integer :: lb(3), ub(3)

    this%levels(l)%boxes(b)%level = l
    this%levels(l)%boxes(b)%number = b

    lb = -this%ghost_cells + 1
    ub = dims + this%ghost_cells

    allocate ( this%levels(l)%boxes(b)%flags ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    allocate ( this%levels(l)%boxes(b)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ) )

    this%levels(l)%nboxes = this%levels(l)%nboxes + 1

    this%levels(l)%boxes(b)%dims = dims
    this%levels(l)%boxes(b)%left_edge(:) = ledges(:)
    this%levels(l)%boxes(b)%right_edge(:) = redges(:)
  end subroutine rhyme_samr_init_box
end submodule rhyme_samr_init_box_submodule
