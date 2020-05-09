submodule(rhyme_uv_background) rhyme_mh_init_smod
contains
module subroutine rhyme_uv_background_init(uvb, logger)
   implicit none

   type(uv_background_t), intent(inout) :: uvb
   type(logger_t), intent(inout) :: logger

   character(len=128) :: uvb_str

   call logger%begin_section('uv_background')

   write (uvb_str, *) uvb
   call logger%log('', 'uv_background', '=', [uvb_str])

   call logger%end_section
end subroutine rhyme_uv_background_init
end submodule rhyme_mh_init_smod
