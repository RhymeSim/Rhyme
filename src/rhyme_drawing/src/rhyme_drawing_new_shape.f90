submodule ( rhyme_drawing ) new_shape_smod
contains
  module function rhyme_drawing_new_shape ( this, shape_type ) result ( shape )
    implicit none

    class ( drawing_t ), intent(inout) :: this
    integer, intent(in) :: shape_type

    type ( shape_t ), pointer :: shape

    shape => this%shapes

    if ( associated ( shape ) ) then
      do while ( associated ( shape%next ) )
        shape => shape%next
      end do

      allocate( shape%next )
      shape => shape%next
    else
      allocate( this%shapes )
      shape => this%shapes
    end if

    shape%type = shape_type

    shape%cuboid%left_corner = 0
    shape%cuboid%lengths = 0

    shape%sphere%origin = 0.d0
    shape%sphere%r = 0.d0

#if NDIM > 1
    shape%prism%vertices = 0.d0

    shape%slab_2d%pos = 0.d0
    shape%slab_2d%sigma = 0.d0

#if NDIM > 2
    shape%prism%thickness = 0.d0
#endif
#endif

    shape%fill%type = drid%unset
    shape%fill%colors( :, 1 ) = 0.d0
    shape%fill%colors( :, 2 ) = 0.d0
  end function rhyme_drawing_new_shape
end submodule new_shape_smod
