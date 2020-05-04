module rhyme_physics_factory
   use rhyme_physics

   implicit none

   character(len=32), parameter, private :: rho_str_param = 'kg / m^3'
   character(len=32), parameter, private :: length_str_param = 'm'
   character(len=32), parameter, private :: time_str_param = 's'

   type rhyme_physics_factory_t
   contains
      procedure :: generate => rhyme_physics_factory_generate
   end type rhyme_physics_factory_t

   type(rhyme_physics_factory_t) :: ph_factory = rhyme_physics_factory_t()

contains
   function rhyme_physics_factory_generate(this, factory_type) result(physics)
      implicit none

      class(rhyme_physics_factory_t), intent(inout) :: this
      character(len=*), intent(in) :: factory_type

      type(physics_t) :: physics

      if (factory_type == 'SI') then
         physics%rho_str = rho_str_param
         physics%length_str = length_str_param
         physics%time_str = time_str_param
      else
         print *, 'Unknow physcis factory type!'
      end if
   end function rhyme_physics_factory_generate
end module rhyme_physics_factory
