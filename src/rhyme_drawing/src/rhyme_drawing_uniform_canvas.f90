submodule ( rhyme_drawing ) rhyme_drawing_uniform_canvas_submodule
contains
  module subroutine rhyme_drawing_uniform_canvas ( samr, ig, bg_prim )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_primitive_t ), intent ( in ) :: bg_prim

    integer :: l, b, d(3)
    type ( hydro_conserved_t ) :: bg

    call ig%prim_to_cons( bg_prim, bg )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        d = samr%levels(l)%boxes(b)%dims
        samr%levels(l)%boxes(b)%hydro( 1:d(1), 1:d(2), 1:d(3) ) = bg
      end do
    end do

  end subroutine rhyme_drawing_uniform_canvas
end submodule rhyme_drawing_uniform_canvas_submodule
