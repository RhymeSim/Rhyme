module rhyme_hdf5_util_factory
   use rhyme_hdf5_util

contains

   function hdf5_util_factory_generate(factory_type) result(h5)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(hdf5_util_t) :: h5

      if (factory_type == 'empty') then
         h5%filename = ''
         h5%fid = h5id%unset
      else
         print *, 'Unknown chombo factory type!', factory_type
      end if
   end function hdf5_util_factory_generate
end module rhyme_hdf5_util_factory
