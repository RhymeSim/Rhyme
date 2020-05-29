module rhyme_initial_condition_factory
   use rhyme_initial_condition

contains

   function initial_condition_factory_generate(factory_type) result(ic)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(initial_condition_t) :: ic

      integer :: l, nlevels

      nlevels = 0

      if (factory_type == 'uniform') then
         nlevels = 1
      else if (factory_type == '4levels') then
         nlevels = 4
      else
         print *, 'Unknown initial condition factory type!', factory_type
      end if

      ic%type = icid%simple
      ic%snapshot_type = icid%unset
      ic%snapshot_path = ''
      ic%box_length_unit = 'm'

      ic%nlevels = nlevels
      ic%max_nboxes = 0
      ic%max_nboxes(0:nlevels - 1) = [(l**3, l=1, nlevels)]
      ic%redshift = 1.37d0

#if NDIM == 1
      ic%base_grid = [1024]
      ic%box_lengths%v = [1d0]
#elif NDIM == 2
      ic%base_grid = [128, 128]
      ic%box_lengths(1)%v = 1d0
      ic%box_lengths(2)%v = 1d0
#elif NDIM == 3
      ic%base_grid = [64, 64, 8]
      ic%box_lengths(1)%v = 1d0
      ic%box_lengths(2)%v = 1d0
      ic%box_lengths(3)%v = .125d0
#endif

      ic%redshift = 0d0

   end function initial_condition_factory_generate
end module rhyme_initial_condition_factory
