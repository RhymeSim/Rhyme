module rhyme_cfl_factory
   use rhyme_cfl

contains

   function cfl_factory_generate(courant_number) result(cfl)
      implicit none

      real(kind=8), intent(in), optional :: courant_number

      type(cfl_t) :: cfl

      if (present(courant_number)) then
         cfl%courant_number = courant_number
      else
         cfl%courant_number = 2d-1
      end if
   end function cfl_factory_generate
end module rhyme_cfl_factory
