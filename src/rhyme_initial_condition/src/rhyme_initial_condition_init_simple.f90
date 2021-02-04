submodule(rhyme_initial_condition) rhyme_ic_init_simple_smod
contains
   module subroutine rhyme_initial_condition_init_simple(ic, samr, units, logger)
      implicit none

      type(initial_condition_t), intent(in) :: ic
      type(samr_t), intent(inout) :: samr
      type(units_t), intent(in) :: units
      type(logger_t), intent(inout) :: logger

#if NDIM == 1
#define LEDGES_ARRAY [ 1 ]
#elif NDIM == 2
#define LEDGES_ARRAY [ 1, 1 ]
#elif NDIM == 3
#define LEDGES_ARRAY [ 1, 1, 1 ]
#endif

      integer :: l, d

      call logger%begin_section('simple')

      if (any(ic%base_grid .eq. icid%unset) &
          .or. ic%nlevels .eq. icid%unset) then
         call logger%err('ic_base_grid or ic_nlevels is not set')
         return
      end if

      samr%nlevels = ic%nlevels
      samr%base_grid = ic%base_grid
      samr%ghost_cells = merge(2, 0, samr%base_grid > 1)

      do d = 1, NDIM
         samr%box_lengths(d) = rhyme_nombre_get_value( &
                               ic%box_lengths(d) .to.units%length)
      end do

      samr%max_nboxes = ic%max_nboxes
      samr%max_nboxes(samr%nlevels:) = 0

      samr%levels%level = [(l, l=0, 23)]
      samr%levels%nboxes = 0
      samr%levels%refine_factor = 2.d0
      samr%levels%max_nboxes = samr%max_nboxes

      do l = 0, samr%nlevels - 1
         samr%levels(l)%dx = samr%box_lengths/(samr%base_grid*2.d0**l)
         allocate (samr%levels(l)%boxes(samr%max_nboxes(l)))
      end do

      samr%initialized = .true.

      ! Initializing the first level
      call rhyme_samr_init_box(samr, 0, 1, samr%base_grid, &
                               LEDGES_ARRAY, samr%base_grid)

      call logger%end_section
   end subroutine rhyme_initial_condition_init_simple
end submodule rhyme_ic_init_simple_smod
