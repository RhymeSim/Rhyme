module rhyme_uv_background_factory
   use rhyme_uv_background

contains

   function uv_background_factory_generate(factory_type) result(uvb)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(uv_background_t) :: uvb
      uvb = uv_background_t()

      if (factory_type == 'default') then
      else if (factory_type == 'HM') then
         uvb%model = uvbid%HM12
      else
         print *, 'Unknown uv_background factory type!', factory_type
      end if
   end function uv_background_factory_generate
end module rhyme_uv_background_factory
