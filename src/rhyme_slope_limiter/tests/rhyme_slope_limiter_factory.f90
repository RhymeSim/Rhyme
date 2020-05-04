module rhyme_slope_limiter_factory
   use rhyme_slope_limiter

contains

   function slope_limiter_factory_generate(factory_type) result(sl)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(slope_limiter_t) :: sl

      sl%w = 0d0

      if (factory_type == 'vanLeer') then
         sl%type = slid%van_Leer
      else if (factory_type == 'minmod') then
         sl%type = slid%minmod
      else if (factory_type == 'vanAlbada') then
         sl%type = slid%van_albada
      else if (factory_type == 'superbee') then
         sl%type = slid%superbee
      else
         print *, 'Unknown slope limiter factory type!', factory_type
      end if
   end function slope_limiter_factory_generate
end module rhyme_slope_limiter_factory
