submodule(rhyme_param_parser) rhyme_param_parser_occurences_submodule
contains
   module function rhyme_param_parser_occurences(this, key) result(occur)
      implicit none

      class(config_t), intent(in) :: this
      character(len=*), intent(in) :: key
      integer :: occur

      character(len=32) :: keyword
      integer :: ios

      open (123, file=this%path, action='read', form="formatted")

      occur = 0

      do
         read (123, *, iostat=ios) keyword
         if (ios .ne. 0) exit
         if (trim(adjustl(keyword)) .eq. trim(key)) occur = occur + 1
      end do

      close (123)
   end function rhyme_param_parser_occurences
end submodule rhyme_param_parser_occurences_submodule
