submodule(rhyme_nombre) mul_smod
contains
module function rhyme_nombre_mul(mul, n) result(n_new)
   implicit none

   class(*), intent(in) :: mul
   type(nombre_t), intent(in) :: n
   type(nombre_t) :: n_new

   type(nombre_unit_t), pointer :: u

   u => .clonechain.n%u

   select type (m=>mul)
   type is (integer)
      n_new = nombre_t(n%v*m, u)
   type is (real(kind=4))
      n_new = nombre_t(n%v*real(m, kind=8), u)
   type is (real(kind=8))
      n_new = nombre_t(n%v*m, u)
   end select
end function rhyme_nombre_mul

module function rhyme_nombre_mul_rev(n, mul) result(n_new)
   implicit none

   type(nombre_t), intent(in) :: n
   class(*), intent(in) :: mul
   type(nombre_t) :: n_new

   type(nombre_unit_t), pointer :: u, u_mul

   u => .clonechain.n%u

   select type (m=>mul)
   type is (nombre_t)
      u_mul => .clonechain.m%u
      n_new = nombre_t(m%v*n%v, u*u_mul)
   type is (integer)
      n_new = nombre_t(real(m, kind=8)*n%v, u)
   type is (real(kind=4))
      n_new = nombre_t(real(m, kind=8)*n%v, u)
   type is (real(kind=8))
      n_new = nombre_t(m*n%v, u)
   end select
end function rhyme_nombre_mul_rev
end submodule mul_smod
