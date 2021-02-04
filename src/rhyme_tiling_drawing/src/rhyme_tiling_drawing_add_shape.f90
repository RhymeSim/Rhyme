submodule(rhyme_tiling_drawing) add_shape_smod
contains

   module function rhyme_tiling_drawing_add_shape(this, shape_type) result(shape)
      implicit none

      class(tiling_drawing_t), intent(inout) :: this
      integer, intent(in) :: shape_type

      type(tiling_shape_t), pointer :: shape

      shape => this%shapes

      if (associated(shape)) then
         do while (associated(shape%next))
            shape => shape%next
         end do

         allocate (shape%next)
         shape => shape%next
      else
         allocate (this%shapes)
         shape => this%shapes
      end if

      shape%type = shape_type
   end function rhyme_tiling_drawing_add_shape
end submodule add_shape_smod
