module rhyme_deep_rs
   use rhyme_hdf5_util
   use rhyme_units
   use rhyme_logger

   implicit none

   type, private :: indices_t
   end type indices_t

   type(indices_t), parameter :: drsid = indices_t()

   type deep_rs_t
      real(kind=4), allocatable, dimension(:, :) :: w1, w2, w3
      real(kind=4), allocatable, dimension(:, :) :: b1, b2, b3
      real(kind=4) :: drho = 0e0, dp = 0e0, dv = 0e0
      real(kind=4) :: rho_conv = 0e0, p_conv = 0e0, v_conv = 0e0
      integer :: n_layers = 0
      character(len=1024) :: path = "./v20210616.h5"
      character(len=1024) :: d_norm = "", p_norm = "", v_norm = ""
   contains
      procedure :: rhyme_deep_rs_write_formatted
      generic :: write (formatted) => rhyme_deep_rs_write_formatted
   end type deep_rs_t

   interface
      module subroutine rhyme_deep_rs_init(drs, units, logger)
         type(deep_rs_t), intent(inout) :: drs
         type(units_t), intent(in) :: units
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_deep_rs_init

      pure module function rhyme_deep_rs_exec(drs, r1, p1, r2, p2, dv) result(p)
         type(deep_rs_t), intent(in) :: drs
         real(kind=8), intent(in) :: r1, p1, r2, p2, dv
         real(kind=8) :: p
      end function rhyme_deep_rs_exec
   end interface

contains
   subroutine rhyme_deep_rs_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(deep_rs_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<deep_rs_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_deep_rs_write_formatted
end module rhyme_deep_rs
