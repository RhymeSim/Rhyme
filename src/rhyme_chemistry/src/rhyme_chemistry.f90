module rhyme_chemistry
   use rhyme_periodic_table
   use rhyme_physics
   use rhyme_logger

   implicit none

   type, private :: indices_t
   end type indices_t

   type(indices_t), parameter :: chemid = indices_t()

   type(periodic_table_t), private :: periodic_table

   type chemistry_t
      real(kind=8) :: rho_to_number_density = 0d0

      character(len=8), dimension(NELE) :: element_names = ''
      real(kind=8), dimension(NELE) :: element_abundances = 0d0
      type(periodic_table_element_t), dimension(NELE) :: elements
   contains
      procedure :: rhyme_chemistry_write_formatted
      generic :: write (formatted) => rhyme_chemistry_write_formatted
   end type chemistry_t

   interface
      module subroutine rhyme_chemistry_init(chemistry, physics, logger)
         type(chemistry_t), intent(inout) :: chemistry
         type(physics_t), intent(in) :: physics
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_chemistry_init

      pure module function rhyme_chemistry_ne(chemistry, density, ntr_frac) result(ne)
         type(chemistry_t), intent(in) :: chemistry
         real(kind=8), intent(in) :: density, ntr_frac(:)
         real(kind=8) :: ne
      end function rhyme_chemistry_ne

      pure module function rhyme_chemistry_one_over_mu(chemistry, ntr_frac) result(one_over_mu)
         type(chemistry_t), intent(in) :: chemistry
         real(kind=8), intent(in) :: ntr_frac(NSPE)
         real(kind=8) :: one_over_mu
      end function rhyme_chemistry_one_over_mu
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
