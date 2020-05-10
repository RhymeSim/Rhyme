submodule(rhyme_uv_background) get_smod
implicit none
contains
real(kind=4) pure function rhyme_uv_background_get(this, z, species) result(rate)
   implicit none

   class(uv_background_t), intent(in) :: this
   real(kind=8), intent(in) :: z
   character(len=*), intent(in) :: species

   integer :: table_size

   select case (species)
   case ('HI')
   case ('HeI')
   case ('HeII')
   case default
      rate = 0e0
   end select
end function rhyme_uv_background_get
end submodule get_smod
