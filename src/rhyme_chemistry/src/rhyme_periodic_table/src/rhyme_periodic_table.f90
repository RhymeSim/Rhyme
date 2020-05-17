module rhyme_periodic_table
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
      integer :: H = 1, He = 2
   end type indices_t

   type(indices_t), parameter :: ptid = indices_t()

   type, private :: species_t
      character(len=16) :: symb = ''
      integer :: ionized = 0
      ! Recombination ionisation rate (case A) [cm^3 s^-1]
      procedure(rate_i), pointer, nopass :: RI_A => null()
      ! Recombination ionisation rate (case B) [cm^3 s^-1]
      procedure(rate_i), pointer, nopass :: RI_B => null()
      ! Collisional ionisation rate [cm^3 s^-1]
      procedure(rate_i), pointer, nopass :: CI => null()
      ! Collisional ionisation equilibrium (case A) [Neutral fraction]
      procedure(collisional_equilibrium_i), pointer, nopass :: CIE_A => null()
      ! Collisional ionisation equilibrium (case B) [Neutral fraction]
      procedure(collisional_equilibrium_i), pointer, nopass :: CIE_B => null()
      ! Ionization equilibrium (case A) [Neutral fraction]
      procedure(ionisation_equilibrium_i), pointer, nopass :: CPIE_A => null()
      ! Ionization equilibrium (case B) [Neutral fraction]
      procedure(ionisation_equilibrium_i), pointer, nopass :: CPIE_B => null()
      ! Number of electrons
      procedure(number_of_electron_i), pointer, nopass :: ne => null()
      ! Ionisation fraction
      procedure(ionization_fraction_i), pointer, nopass :: f => null()
   end type species_t

   type periodic_table_element_t
      character(len=8) :: symb = ''
      integer :: atomic_number = 0
      real(kind=8) :: atomic_weight = 0e0

      integer :: nspecies = 0
      type(species_t), allocatable :: species(:)

      procedure(number_of_electron_i), pointer, nopass :: ne => null()
   end type periodic_table_element_t

   type periodic_table_t
      type(periodic_table_element_t) :: elements(2)
   contains
      procedure :: rhyme_periodic_table_write_formatted
      generic :: write (formatted) => rhyme_periodic_table_write_formatted
      procedure :: get_element_by_name => rhyme_periodic_table_get_element_by_name
   end type periodic_table_t

   interface
      module subroutine rhyme_periodic_table_init(pt, logger)
         type(periodic_table_t), intent(inout) :: pt
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_periodic_table_init

      module function rhyme_periodic_table_get_element_by_name(pt, element_symb) result(element)
         class(periodic_table_t), intent(in) :: pt
         character(len=*), intent(in) :: element_symb
         type(periodic_table_element_t) :: element
      end function rhyme_periodic_table_get_element_by_name
   end interface

   abstract interface
      pure function rate_i(T) result(rate)
         real(kind=8), intent(in) :: T
         real(kind=8) :: rate
      end function rate_i

      pure function collisional_equilibrium_i(T) result(neutral_fraction)
         real(kind=8), intent(in) :: T
         real(kind=8) :: neutral_fraction
      end function collisional_equilibrium_i

      pure function ionisation_equilibrium_i(T, Gamma_photo, ne) result(neutral_fraction)
         real(kind=8), intent(in) :: T, Gamma_photo(:), ne
         real(kind=8) :: neutral_fraction
      end function ionisation_equilibrium_i

      pure function number_of_electron_i(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)
         real(kind=8) :: ne
      end function number_of_electron_i

      pure function ionization_fraction_i(ntr_frac) result(f)
         real(kind=8), intent(in) :: ntr_frac(:)
         real(kind=8) :: f
      end function ionization_fraction_i
   end interface

contains
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
