submodule(rhyme_string) rhyme_string_is_nan_smod
contains
pure elemental module function rhyme_string_is_nan(input) result(is_nan)
   use, intrinsic :: ieee_arithmetic

   implicit none

   class(*), intent(in) :: input
   logical :: is_nan

   select type (inp => input)
   type is (real(kind=4))
      if (ieee_is_nan(inp)) then
         is_nan = .true.
      else
         is_nan = .false.
      end if

   type is (real(kind=8))
      if (ieee_is_nan(inp)) then
         is_nan = .true.
      else
         is_nan = .false.
      end if

   class default
      is_nan = .false.
   end select
end function rhyme_string_is_nan
end submodule rhyme_string_is_nan_smod
