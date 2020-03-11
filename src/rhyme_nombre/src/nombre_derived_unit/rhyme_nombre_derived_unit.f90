module rhyme_nombre_derived_unit
   use rhyme_nombre_base_unit_chain

   implicit none

   type nombre_unit_t
      type(nombre_prefix_t) :: prefix
      character(len=8) :: symb = ''
      real(kind=8) :: conv = 1d0
      type(nombre_dimension_t) :: dim
      real(kind=8) :: pow = 1d0
      type(nombre_unit_t), pointer :: next => null(), prev => null()
      type(nombre_base_unit_t), pointer :: head => null()
   contains
      procedure :: rhyme_nombre_derived_unit_write_formatted
      generic :: write (formatted) => rhyme_nombre_derived_unit_write_formatted
   end type nombre_unit_t

   ! Mass
   type(nombre_unit_t), pointer :: solar_mass, hydrogen_mass, atomic_mass_unit

   ! Length
   type(nombre_unit_t), pointer :: parsec, light_year, astronomical_unit

   ! Time
   type(nombre_unit_t), pointer :: year

   ! Energy
   type(nombre_unit_t), pointer :: joule, electron_volt

   ! Power
   type(nombre_unit_t), pointer :: watt

   ! Pressure
   type(nombre_unit_t), pointer :: pascal

   ! Frequency
   type(nombre_unit_t), pointer :: hertz

   ! Angle
   type(nombre_unit_t), pointer :: radian, stradian

   ! Force
   type(nombre_unit_t), pointer :: newton

   type(nombre_unit_t) :: derived_units(15)

   interface
      module subroutine rhyme_nombre_derived_unit_init()
      end subroutine rhyme_nombre_derived_unit_init

      module function rhyme_nombre_derived_unit_new() result(du)
         type(nombre_unit_t), pointer :: du
      end function rhyme_nombre_derived_unit_new

      module function rhyme_nombre_derived_unit_clone(du) result(du_new)
         type(nombre_unit_t), target, intent(in) :: du
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_clone

      module function rhyme_nombre_derived_unit_equality(du1, du2) result(eq)
         type(nombre_unit_t), intent(in) :: du1, du2
         logical :: eq
      end function rhyme_nombre_derived_unit_equality

      module function rhyme_nombre_derived_unit_update_symbol(du, s) result(du_new)
         type(nombre_unit_t), intent(in) :: du
         character(len=*), intent(in) :: s
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_update_symbol

      module function rhyme_nombre_derived_unit_print(du) result(str)
         type(nombre_unit_t), intent(in) :: du
         character(len=64) :: str
      end function rhyme_nombre_derived_unit_print

      module function rhyme_nombre_derived_unit_parse(str) result(dunit)
         character(len=*), intent(in) :: str
         type(nombre_unit_t), pointer :: dunit
      end function rhyme_nombre_derived_unit_parse

      module function rhyme_nombre_derived_unit_mul_ibuc(i, buc) result(du_new)
         integer, intent(in) :: i
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_mul_ibuc

      module function rhyme_nombre_derived_unit_mul_rbuc(r, buc) result(du_new)
         real(kind=4), intent(in) :: r
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_mul_rbuc

      module function rhyme_nombre_derived_unit_mul_r8buc(r8, buc) result(du_new)
         real(kind=8), intent(in) :: r8
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_mul_r8buc

      module function rhyme_nombre_derived_unit_div_ibuc(i, buc) result(du_new)
         integer, intent(in) :: i
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_div_ibuc

      module function rhyme_nombre_derived_unit_div_rbuc(r, buc) result(du_new)
         real(kind=4), intent(in) :: r
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_div_rbuc

      module function rhyme_nombre_derived_unit_div_r8buc(r8, buc) result(du_new)
         real(kind=8), intent(in) :: r8
         type(nombre_base_unit_t), target, intent(in) :: buc
         type(nombre_unit_t), pointer :: du_new
      end function rhyme_nombre_derived_unit_div_r8buc
   end interface

   interface operator(*)
      module procedure rhyme_nombre_derived_unit_mul_ibuc
      module procedure rhyme_nombre_derived_unit_mul_rbuc
      module procedure rhyme_nombre_derived_unit_mul_r8buc
   end interface operator(*)

   interface operator(/)
      module procedure rhyme_nombre_derived_unit_div_ibuc
      module procedure rhyme_nombre_derived_unit_div_rbuc
      module procedure rhyme_nombre_derived_unit_div_r8buc
   end interface operator(/)

   interface operator(==)
      module procedure rhyme_nombre_derived_unit_equality
   end interface operator(==)

   interface operator(.as.)
      module procedure rhyme_nombre_derived_unit_update_symbol
   end interface operator(.as.)

   interface operator(.clone.)
      module procedure rhyme_nombre_derived_unit_clone
   end interface operator(.clone.)

   interface operator(.print.)
      module procedure rhyme_nombre_derived_unit_print
   end interface operator(.print.)

contains
   subroutine rhyme_nombre_derived_unit_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(nombre_unit_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,A,A,I0,A,A,ES10.3,A,ES10.3,A,L,A,L,A,A,A,A,A,A,A,I0,A)', &
             iostat=iostat, iomsg=iomsg) &
         '<nombre_unit_t', &
         ' symb="', adjustl(trim(this%symb)), '"', &
         ' prefix=("', adjustl(trim(this%prefix%symb)), '", ', this%prefix%base_10, ')', &
         ' conv=', this%conv, &
         ' pow=', this%pow, &
         ' next=', associated(this%next), &
         ' prev=', associated(this%prev), &
         ' units="', trim(.printchain.this%head), '"', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         ' >'
   end subroutine rhyme_nombre_derived_unit_write_formatted
end module rhyme_nombre_derived_unit
