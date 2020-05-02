module rhyme_periodic_table
   use rhyme_nombre
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
      integer :: H = 1, He = 2
   end type indices_t

   type(indices_t), parameter :: ptid = indices_t()

   abstract interface
      pure function rate_interface(T) result(rate)
         real(kind=8), intent(in) :: T
         real(kind=8) :: rate
      end function rate_interface
   end interface

   type element_species_t
      character(len=16) :: symb = ''
      integer :: ionized = 1
      procedure(rate_interface), pointer, nopass :: RI_A => null()  ! Recombination ionization rate (case A)
      procedure(rate_interface), pointer, nopass :: RI_B => null()  ! Recombination ionization rate (case B)
      procedure(rate_interface), pointer, nopass :: CI => null()  ! Collisional ionization rate
      type(element_species_t), pointer :: prev => null(), next => null()
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
      procedure :: get_species_by_name => rhyme_periodic_table_get_species_by_name
   end type periodic_table_t

   interface
      module subroutine rhyme_periodic_table_init(pt, logger)
         type(periodic_table_t), intent(inout) :: pt
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_periodic_table_init

      module function rhyme_periodic_table_get_species_by_name(pt, species_name) result(species)
         class(periodic_table_t), intent(in) :: pt
         character(len=*), intent(in) :: species_name
         type(element_species_t), pointer :: species
      end function rhyme_periodic_table_get_species_by_name
   end interface

   interface operator(.getspeciesbyname.)
      procedure rhyme_periodic_table_get_species_by_name
   end interface operator(.getspeciesbyname.)

contains
   subroutine rhyme_periodic_table_element_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(periodic_table_element_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' symb=', trim(this%symb), &
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

      character(len=1024) :: str_elements = ''
      integer :: ei

      do ei = 1, size(this%elements)
         if (this%elements(ei)%symb == '') cycle
         write (str_elements, '(A,A2,A)') trim(str_elements), ', ', trim(this%elements(ei)%symb)
      end do

      write (unit, fmt='(A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' elements=', trim(str_elements), &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_periodic_table_write_formatted
end module rhyme_periodic_table
