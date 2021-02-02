module rhyme_sanity_check
   use rhyme_nombre
   use rhyme_units
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: rho = 1, vx = 2, vy = 3, vz = 4, e_tot = 5, temp = 6, &
                 ntr_frac_0 = 7, ntr_frac_1 = 8, ntr_frac_2 = 9, total_mass = 10, &
                 total_energy = 11, abs_v = 12, mach = 13
   end type indices_t

   type(indices_t), parameter :: scid = indices_t()

   type sanity_check_t
      logical :: enabled = .true.
      integer :: every = 1

      logical :: properties(13) = .false.

      real(kind=8) :: rho_range(2) = [0d0, huge(0d0)]
      character(len=128) :: rho_unit_str = ''
      type(nombre_unit_t), pointer :: rho_unit => null()

      real(kind=8) :: vx_range(2) = [-huge(0d0), huge(0d0)]
      character(len=128) :: vx_unit_str = ''
      type(nombre_unit_t), pointer :: vx_unit => null()

      real(kind=8) :: vy_range(2) = [-huge(0d0), huge(0d0)]
      character(len=128) :: vy_unit_str = ''
      type(nombre_unit_t), pointer :: vy_unit => null()

      real(kind=8) :: vz_range(2) = [-huge(0d0), huge(0d0)]
      character(len=128) :: vz_unit_str = ''
      type(nombre_unit_t), pointer :: vz_unit => null()

      real(kind=8) :: e_tot_range(2) = [0d0, huge(0d0)]
      character(len=128) :: e_tot_unit_str = ''
      type(nombre_unit_t), pointer :: e_tot_unit => null()

      real(kind=8) :: temp_range(2) = [0d0, huge(0d0)]
      character(len=128) :: temp_unit_str = ''
      type(nombre_unit_t), pointer :: temp_unit => null()

      real(kind=8) :: ntr_frac_0_range(2) = [0d0, 1d0]
      real(kind=8) :: ntr_frac_1_range(2) = [0d0, 1d0]
      real(kind=8) :: ntr_frac_2_range(2) = [0d0, 1d0]

      real(kind=8) :: abs_v_range(2) = [0d0, huge(0d0)]
      character(len=128) :: abs_v_unit_str = ''
      type(nombre_unit_t), pointer :: abs_v_unit => null()

      real(kind=8) :: mach_range(2) = [0d0, huge(0d0)]
   contains
      procedure :: rhyme_sanity_check_write_formatted
      generic :: write (formatted) => rhyme_sanity_check_write_formatted
   end type sanity_check_t

   interface
      module subroutine rhyme_sanity_check_init(sc, units, logger)
         type(sanity_check_t), intent(inout) :: sc
         type(units_t), intent(in) :: units
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_sanity_check_init
   end interface

contains
   subroutine rhyme_sanity_check_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(sanity_check_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,L,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<sanity_check_t', &
         ' enabled=', this%enabled, &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_sanity_check_write_formatted
end module rhyme_sanity_check
