module rhyme_initial_condition_factory
   use rhyme_initial_condition

contains

   function initial_condition_factory_generate(factory_type) result(ic)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(initial_condition_t) :: ic

      integer :: l, nlevels

      if (factory_type == 'uniform') then
         nlevels = 1
      else if (factory_type == '4levels') then
         nlevels = 4
      else
         nlevels = 1
         print *, 'Unknown initial condition factory type!', factory_type
      end if

      ic%type = icid%simple
      ic%snapshot_type = icid%unset
      ic%snapshot_path = ''
      ic%box_length_unit = 'm'

      ic%nlevels = nlevels
      ic%max_nboxes = 0
      ic%max_nboxes(0:nlevels - 1) = [(l**3, l=1, nlevels)]

#if NDIM == 1
      ic%base_grid = [16]
      ic%box_lengths%v = [1.d0]
#elif NDIM == 2
      ic%base_grid = [16, 8]
      ic%box_lengths%v = [1.d0, .5d0]
#elif NDIM == 3
      ic%base_grid = [16, 8, 4]
      ic%box_lengths%v = [1.d0, .5d0, .25d0]
#endif

   end function initial_condition_factory_generate
end module rhyme_initial_condition_factory
