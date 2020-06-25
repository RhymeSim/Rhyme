module rhyme_tiling_drawing_factory
   use rhyme_tiling_drawing

contains

   function tiling_drawing_factory_generate(factory_type) result(draw)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(tiling_drawing_t) :: draw

      select case (factory_type)
      case ('defulat')
         draw = tiling_drawing_t()
      case default
         print *, 'Unknown tiling_drawing factory type!', factory_type
      end select
   end function tiling_drawing_factory_generate
end module rhyme_tiling_drawing_factory
