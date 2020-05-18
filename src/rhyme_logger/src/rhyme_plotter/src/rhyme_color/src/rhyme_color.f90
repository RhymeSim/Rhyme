module rhyme_color
   implicit none

   type, private :: rhyme_color_indices_t
      character(len=1) :: esc = achar(27)
      character(len=2) :: start = achar(27)//'['
      character(len=4) :: clear = achar(27)//'[0m'
   end type rhyme_color_indices_t

   type(rhyme_color_indices_t), parameter :: clrid = rhyme_color_indices_t()

   type, private :: rhyme_colorscheme_indices_t
      integer :: len = 4
      integer :: magma_grey = 1
      integer :: rainbow = 2
      integer :: smooth_rainbow = 3
      integer :: viridis = 4
      integer :: unknown = -2 ! color for values below the range of the cs
      integer :: low_end = -1 ! color for values below the range of the cs
      integer :: high_end = 0 ! color for values above the range of the cs
      integer :: pallet_len = 32 ! Maximum length of the colorscheme pallet
      character(len=17) :: char_color_fmt = '(A9,A1,A4)'
   end type rhyme_colorscheme_indices_t

   type(rhyme_colorscheme_indices_t), parameter :: csid = rhyme_colorscheme_indices_t()

   type, private :: terminal_color_t
      character(len=9) :: black = clrid%start//"38;5;0m"
      character(len=9) :: blk = clrid%start//"38;5;0m"
      character(len=9) :: red = clrid%start//"38;5;1m"
      character(len=9) :: rd = clrid%start//"38;5;1m"
      character(len=9) :: green = clrid%start//"38;5;2m"
      character(len=9) :: gn = clrid%start//"38;5;2m"
      character(len=9) :: yellow = clrid%start//"38;5;3m"
      character(len=9) :: yl = clrid%start//"38;5;3m"
      character(len=9) :: blue = clrid%start//"38;5;4m"
      character(len=9) :: bl = clrid%start//"38;5;4m"
      character(len=9) :: magenta = clrid%start//"38;5;5m"
      character(len=9) :: mg = clrid%start//"38;5;5m"
      character(len=9) :: cyan = clrid%start//"38;5;6m"
      character(len=9) :: cy = clrid%start//"38;5;6m"
      character(len=9) :: white = clrid%start//"38;5;7m"
      character(len=9) :: wh = clrid%start//"38;5;7m"
      character(len=4) :: nc = clrid%clear
   end type terminal_color_t

   type(terminal_color_t), parameter :: tc = terminal_color_t()

   type color_t
      integer :: r, g, b
      integer :: n
      character(len=11) :: fg, bg
   contains
      procedure :: rhyme_color_write_formatted
      generic :: write (formatted) => rhyme_color_write_formatted
   end type color_t

   type(color_t), dimension(0:255) :: colors

   type colorscheme_t
      integer :: n = 0
      type(color_t), dimension(-2:csid%pallet_len) :: pallet
      character(len=1), dimension(-2:csid%pallet_len) :: chars = '0'
   end type colorscheme_t

   type(colorscheme_t), dimension(csid%len) :: colorschemes

   interface
      module subroutine rhyme_color_init()
      end subroutine rhyme_color_init

      pure module function rhyme_color_equality(clr1, clr2) result(eq)
         type(color_t), intent(in) :: clr1, clr2
         logical :: eq
      end function rhyme_color_equality
   end interface

   interface operator(==)
      module procedure rhyme_color_equality
   end interface operator(==)

contains
   subroutine rhyme_color_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(color_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,I0,A,I0,A,I0,A,I0,A,A,A,A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<color_t', &
         ' n=', this%n, &
         ' r=', this%r, &
         ' g=', this%g, &
         ' b=', this%b, &
         ' fg=', this%fg//this%fg(2:)//clrid%clear, &
         ' bg=', this%bg//this%bg(2:)//clrid%clear, &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_color_write_formatted
end module rhyme_color
