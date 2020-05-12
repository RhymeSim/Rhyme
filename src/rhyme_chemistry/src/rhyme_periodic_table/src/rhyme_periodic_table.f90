module rhyme_periodic_table
   use rhyme_nombre
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
      integer :: H = 1, He = 2
   end type indices_t

   type(indices_t), parameter :: ptid = indices_t()

   type species_t
      character(len=8) :: element = ''
      integer :: atomic_number = 0
      type(nombre_t) :: atomic_weight

      character(len=16) :: symb = ''
      integer :: ionized = 1
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
      procedure(ionisation_equilibrium_i), pointer, nopass :: IE_A => null()
      ! Ionization equilibrium (case B) [Neutral fraction]
      procedure(ionisation_equilibrium_i), pointer, nopass :: IE_B => null()

      type(species_t), pointer :: prev => null(), next => null()
   end type species_t

   type periodic_table_t
      type(species_t) :: species(5)
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
         type(species_t) :: species
      end function rhyme_periodic_table_get_species_by_name
   end interface

   interface operator(.getspeciesbyname.)
      procedure rhyme_periodic_table_get_species_by_name
   end interface operator(.getspeciesbyname.)

   abstract interface
      pure function rate_i(T) result(rate)
         real(kind=8), intent(in) :: T
         real(kind=8) :: rate
      end function rate_i

      pure function collisional_equilibrium_i(T) result(neutral_fraction)
         real(kind=8), intent(in) :: T
         real(kind=8) :: neutral_fraction
      end function collisional_equilibrium_i

      pure function ionisation_equilibrium_i(T, Gamma, ne) result(neutral_fraction)
         real(kind=8), intent(in) :: T, Gamma(:), ne
         real(kind=8) :: neutral_fraction
      end function ionisation_equilibrium_i
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
      integer :: si

      do si = 1, size(this%species)
         write (str_elements, '(A,A2,A)') trim(str_elements), ', ', trim(this%species(si)%symb)
      end do

      write (unit, fmt='(A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' elements=', trim(str_elements), &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_periodic_table_write_formatted
end module rhyme_periodic_table
