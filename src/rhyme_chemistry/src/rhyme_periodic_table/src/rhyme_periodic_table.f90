module rhyme_periodic_table
   use rhyme_nombre
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
      integer :: H = 1, He = 2
   end type indices_t

   type(indices_t), parameter :: ptid = indices_t()

   type, private :: element_species_t
      character(len=16) :: symb = ''
      integer :: ionized = 1
      type(element_species_t), pointer :: prev => null(), next => null()
   contains
      procedure :: rhyme_element_species_write_formatted
      generic :: write (formatted) => rhyme_element_species_write_formatted
   end type element_species_t

   type, private :: periodic_table_element_t
      character(len=8) :: symb = ''
      integer :: atomic_number = 0
      type(nombre_t) :: atomic_weight
      type(element_species_t), pointer :: species => null()
   contains
      procedure :: rhyme_periodic_table_element_write_formatted
      generic :: write (formatted) => rhyme_periodic_table_element_write_formatted
   end type periodic_table_element_t

   type periodic_table_t
      type(periodic_table_element_t) :: elements(26)
   contains
      procedure :: rhyme_periodic_table_write_formatted
      generic :: write (formatted) => rhyme_periodic_table_write_formatted
   end type periodic_table_t

   type(periodic_table_t) :: periodic_table

   interface
      module subroutine rhyme_periodic_table_init(pt, logger)
         type(periodic_table_t), intent(inout) :: pt
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_periodic_table_init

      pure module function rhyme_periodic_table_equality(pt1, pt2) result(eq)
         type(periodic_table_t), intent(in) :: pt1, pt2
         logical :: eq
      end function rhyme_periodic_table_equality
   end interface

   interface operator(==)
      module procedure rhyme_periodic_table_equality
   end interface operator(==)

contains
   subroutine rhyme_element_species_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(element_species_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_element_species_write_formatted

   subroutine rhyme_periodic_table_element_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(periodic_table_element_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_periodic_table_element_write_formatted

   subroutine rhyme_periodic_table_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(periodic_table_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_periodic_table_write_formatted
end module rhyme_periodic_table
