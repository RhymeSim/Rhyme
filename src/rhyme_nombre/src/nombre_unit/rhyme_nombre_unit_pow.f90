submodule(rhyme_nombre_unit) pow_smod
contains
   module function rhyme_nombre_unit_pow_duci(duc, i) result(new_duc)
      implicit none

      type(nombre_unit_t), target, intent(in) :: duc
      integer, intent(in) :: i
      type(nombre_unit_t), pointer :: new_duc

      new_duc => .head. (.clonechain.duc)

      do while (associated(new_duc))
         new_duc%pow = new_duc%pow*i

         if (associated(new_duc%next)) then
            new_duc => new_duc%next
         else
            exit
         end if
      end do

      new_duc => .head.new_duc
   end function rhyme_nombre_unit_pow_duci

   module function rhyme_nombre_unit_pow_ducr(duc, r) result(new_duc)
      implicit none

      type(nombre_unit_t), target, intent(in) :: duc
      real(kind=4), intent(in) :: r
      type(nombre_unit_t), pointer :: new_duc

      new_duc => .head. (.clonechain.duc)

      do while (associated(new_duc))
         new_duc%pow = new_duc%pow*real(r, kind=8)

         if (associated(new_duc%next)) then
            new_duc => new_duc%next
         else
            exit
         end if
      end do

      new_duc => .head.new_duc
   end function rhyme_nombre_unit_pow_ducr

   module function rhyme_nombre_unit_pow_ducr8(duc, r8) result(new_duc)
      implicit none

      type(nombre_unit_t), target, intent(in) :: duc
      real(kind=8), intent(in) :: r8
      type(nombre_unit_t), pointer :: new_duc

      new_duc => .head. (.clonechain.duc)

      do while (associated(new_duc))
         new_duc%pow = new_duc%pow*r8

         if (associated(new_duc%next)) then
            new_duc => new_duc%next
         else
            exit
         end if
      end do

      new_duc => .head.new_duc
   end function rhyme_nombre_unit_pow_ducr8
end submodule pow_smod
