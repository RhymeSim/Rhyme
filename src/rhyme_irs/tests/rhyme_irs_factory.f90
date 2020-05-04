module rhyme_irs_factory
   use rhyme_irs

contains

   function irs_factory_generate(factory_type) result(irs)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(irs_t) :: irs

      if (factory_type == 'default') then
         irs = irs_t()
      else
         print *, 'Unknonw IRS factory type!', factory_type
      end if
   end function irs_factory_generate
end module rhyme_irs_factory
