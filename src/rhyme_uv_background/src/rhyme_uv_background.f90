module rhyme_uv_background
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: unset = -1
      integer :: HM12 = 1 ! Haardt & Madau 12
   end type indices_t

   character(len=32), private, parameter :: &
      model_names(-1:1) = [ &
      'unset              ', &
      'none               ', &
      'Haardt & Madau 2012' &
      ]

   type(indices_t), parameter :: uvbid = indices_t()

   type uv_background_t
      integer :: model = uvbid%unset
   contains
      procedure :: rhyme_uv_background_write_formatted
      generic :: write (formatted) => rhyme_uv_background_write_formatted
   end type uv_background_t

   interface
      module subroutine rhyme_uv_background_init(uvb, logger)
         type(uv_background_t), intent(inout) :: uvb
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_uv_background_init

      module function rhyme_uv_background_get(uvb, z, species, logger) result(rates)
         type(uv_background_t), intent(in) :: uvb
         real(kind=8), intent(in) :: z
         character(len=*), intent(in) :: species(:)
         type(logger_t), intent(inout) :: logger
         real(kind=4), dimension(2*size(species)) :: rates
      end function rhyme_uv_background_get

      pure module function rhyme_uv_background_equality(uvb1, uvb2) result(eq)
         type(uv_background_t), intent(in) :: uvb1, uvb2
         logical :: eq
      end function rhyme_uv_background_equality

      module function rhyme_uv_background_haardt_madau_12_get(z, species) result(rates)
         real(kind=8), intent(in) :: z
         character(len=*), intent(in) :: species(:)
         real(kind=4), dimension(2*size(species)) :: rates
      end function rhyme_uv_background_haardt_madau_12_get
   end interface

   interface operator(==)
      module procedure rhyme_uv_background_equality
   end interface operator(==)

contains
   subroutine rhyme_uv_background_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(uv_background_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<uv_background_t', &
         ' model="', trim(model_names(this%model)), '"', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_uv_background_write_formatted
end module rhyme_uv_background
