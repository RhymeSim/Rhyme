module rhyme_chemistry
   use rhyme_periodic_table
   use rhyme_logger

   implicit none

   type, private :: indices_t
   end type indices_t

   type(indices_t), parameter :: chemid = indices_t()

   type chemistry_t
      type(periodic_table_t) :: pt
      ! TODO: read each element and its species separately
      !       This is very important in the case of ionization fractions calculation
      character(len=8), dimension(NSPE) :: species_name = ''
      type(species_t), dimension(NSPE) :: species
   contains
      procedure :: rhyme_chemistry_write_formatted
      generic :: write (formatted) => rhyme_chemistry_write_formatted
   end type chemistry_t

   interface
      module subroutine rhyme_chemistry_init(chem, logger)
         type(chemistry_t), intent(inout) :: chem
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_chemistry_init
   end interface

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
