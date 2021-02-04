submodule(rhyme_nombre) new_smod
contains
   module function rhyme_nombre_new_vdu(val, u) result(n)
      implicit none

      class(*), intent(in) :: val
      type(nombre_unit_t), intent(in), target :: u
      type(nombre_t) :: n

      type(nombre_unit_t), pointer :: u_ptr

      u_ptr => u

      select type (v => val)
      type is (integer)
         n = nombre_t(real(v, kind=8), .head.u_ptr)
      type is (real(kind=4))
         n = nombre_t(real(v, kind=8), .head.u_ptr)
      type is (real(kind=8))
         n = nombre_t(v, .head.u_ptr)
      end select
   end function rhyme_nombre_new_vdu

   module function rhyme_nombre_new_vbu(val, u) result(n)
      implicit none

      class(*), intent(in) :: val
      type(nombre_base_unit_t), intent(in), target :: u
      type(nombre_t) :: n

      type(nombre_unit_t), pointer :: u_ptr

      u_ptr => 1*u

      select type (v => val)
      type is (integer)
         n = nombre_t(real(v, kind=8), .head.u_ptr)
      type is (real(kind=4))
         n = nombre_t(real(v, kind=8), .head.u_ptr)
      type is (real(kind=8))
         n = nombre_t(v, .head.u_ptr)
      end select
   end function rhyme_nombre_new_vbu
end submodule new_smod
