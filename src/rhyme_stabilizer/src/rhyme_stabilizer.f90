module rhyme_stabilizer
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: unset = -1
      integer :: none = 1, linear = 2, quadratic = 3
      character(len=32) :: extrapolation_names(3) = [ &
                           'None     ', &
                           'Linear   ', &
                           'Quadratic' &
                           ]
   end type indices_t

   type(indices_t), parameter :: stid = indices_t()

#if NDIM == 1
#define JCOLON
#define KCOLON
#endif
#if NDIM == 2
#define JCOLON , :
#define KCOLON
#endif
#if NDIM == 3
#define JCOLON , :
#define KCOLON , :
#endif

   real(kind=8), allocatable, private :: rhyme_stabilizer_coords(:JCOLON KCOLON, :)
   real(kind=8), allocatable, private :: rhyme_stabilizer_weights(:JCOLON KCOLON)

   type stabilizer_t
      logical :: enabled = .false.

      integer :: weight = cid%rho
      real(kind=8) :: weight_power = 1

      integer :: extrapolation_type = stid%none

      integer :: max_displacement = 1 ! Pixel
      real(kind=8) :: tolerance = 1d1 ! Pixel

      integer :: max_frequency = 1 ! time steps
      integer :: next_timestep = 0

      logical :: initialize_target = .true.
      real(kind=8) :: target_center(NDIM) = 0d0
   contains
      procedure :: rhyme_stabilizer_write_formatted
      generic :: write (formatted) => rhyme_stabilizer_write_formatted
   end type stabilizer_t

   interface
      module subroutine rhyme_stabilizer_init(st, samr, logger)
         type(stabilizer_t), intent(inout) :: st
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_stabilizer_init

      module function rhyme_stabilizer_displacement_vector(st, samr) result(vec)
         type(stabilizer_t), intent(in) :: st
         type(samr_t), intent(in) :: samr
         real(kind=8) :: vec(NDIM)
      end function rhyme_stabilizer_displacement_vector

      module subroutine rhyme_stabilizer_shifting(st, samr, shift)
         type(stabilizer_t), intent(in) :: st
         type(samr_t), intent(inout) :: samr
         integer, intent(in) :: shift(NDIM)
      end subroutine rhyme_stabilizer_shifting

      module subroutine rhyme_stabilizer_perform(st, samr, logger)
         type(stabilizer_t), intent(in) :: st
         type(samr_t), intent(inout) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_stabilizer_perform
   end interface

contains
   subroutine rhyme_stabilizer_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(stabilizer_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      character(len=32) :: weight_str

      write (weight_str, '(A,A1,F4.2)') trim(cid%labels(this%weight)), '^', this%weight_power

      write (unit, fmt='(A,A,A,A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<stabilizer_t', &
         ' based=', trim(weight_str), &
         ' type="', trim(stid%extrapolation_names(this%extrapolation_type)), '"', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_stabilizer_write_formatted
end module rhyme_stabilizer
