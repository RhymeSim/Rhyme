submodule ( rhyme_drawing ) rhyme_drawing_uniform_rectangle_submoduel
contains
  pure module subroutine rhyme_drawing_uniform_rectangle ( samr, ig, rect )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( shape_t ), intent ( in ) :: rect

    integer :: l, b
    integer :: shift(3), lb(3), ub(3)
    type ( hydro_conserved_t ) :: color

    call ig%prim_to_cons( rect%fill%states(1), color )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        shift = samr%levels(l)%boxes(b)%left_edge

        lb = rect%xl * 2**l - shift
        lb = merge( 1, lb, lb < 1 )
        lb = merge( samr%levels(l)%boxes(b)%dims + 1, lb, lb > samr%levels(l)%boxes(b)%dims )

        ub = ( rect%xl + rect%length ) * 2**l - shift
        ub = merge( 0, ub, ub < 1 )
        ub = merge( samr%levels(l)%boxes(b)%dims, ub, ub > samr%levels(l)%boxes(b)%dims )

        samr%levels(l)%boxes(b)%hydro( lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) ) = color

      end do
    end do
  end subroutine rhyme_drawing_uniform_rectangle
end submodule rhyme_drawing_uniform_rectangle_submoduel
