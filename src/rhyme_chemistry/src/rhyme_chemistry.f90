module rhyme_chemistry
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: chemid = indices_t()

   type chemistry_t
   contains
      procedure :: rhyme_chemistry_write_formatted
      generic :: write (formatted) => rhyme_chemistry_write_formatted
   end type chemistry_t

   interface
      module subroutine rhyme_chemistry_init(chem, logger)
         type(chemistry_t), intent(inout) :: chem
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_chemistry_init

      pure module function rhyme_chemistry_equality(chem1, chem2) result(eq)
         type(chemistry_t), intent(in) :: chem1, chem2
         logical :: eq
      end function rhyme_chemistry_equality
   end interface

   interface operator(==)
      module procedure rhyme_chemistry_equality
   end interface operator(==)

contains
   subroutine rhyme_chemistry_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(chemistry_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<chemistry_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_chemistry_write_formatted
end module rhyme_chemistry