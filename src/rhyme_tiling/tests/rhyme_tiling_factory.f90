module rhyme_tiling_factory
   use rhyme_tiling

contains

   function tiling_factory_generate(factory_type) result(tile)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(tiling_t) :: tile

      select case (factory_type)
      case ('3levels')
         tile%max_level = 3
#if NDIM == 1
         tile%domain = [256]
         tile%lengths = [1d0]
         tile%iteration = 0
         tile%dx = [1d0/256]
         tile%dt = 0d0
         tile%t = 0d0
         tile%tiling_grid = [8]
#elif NDIM == 2
         tile%domain = [128, 64]
         tile%lengths = [2d0, 1d0]
         tile%iteration = 0
         tile%dx = [2d0/128, 1d0/64]
         tile%dt = 0d0
         tile%t = 0d0
         tile%tiling_grid = [4, 2]
#elif NDIM == 3
         tile%domain = [64, 32, 16]
         tile%lengths = [4d0, 2d0, 1d0]
         tile%iteration = 0
         tile%dx = [4d0/64, 2d0/32, 1d0/16]
         tile%dt = 0d0
         tile%t = 0d0
         tile%tiling_grid = [4, 2, 1]
#endif
      case default
         print *, 'Unknown tiling factory type!', factory_type
      end select
   end function tiling_factory_generate
end module rhyme_tiling_factory
