module rhyme_units
  use rhyme_unit

  implicit none


  type(unit_t), dimension(11), parameter :: units_chain = (/ &
    unit_t(one, "m", 1.d0, LengthDim), &
    unit_t(kilo, "g", 1.d0, MassDim), &
    unit_t(one, "s", 1.d0, TimeDim), &
    unit_t(one, "K", 1.d0, TemperatureDim), &
    unit_t(one, "A", 1.d0, ElectricCurrentDim), &
    unit_t(one, "mol", 1.d0, AmountOfSubstanceDim), &
    unit_t(one, "pc", 3.086d16, LengthDim), &
    unit_t(one, "ly", 9.461d15, LengthDim), &
    unit_t(one, "AU", 1.496d11, LengthDim), &
    unit_t(one, "Msun", 1.9885d33, MassDim), &
    unit_t(one, "yr", 3.154d7, TimeDim) &
   /)


  type(unit_t), target :: m = units_chain(1)
  type(unit_t), target :: kg = units_chain(2)
  type(unit_t), target :: s = units_chain(3)
  type(unit_t), target :: Kel = units_chain(4)
  type(unit_t), target :: A = units_chain(5)
  type(unit_t), target :: mol = units_chain(6)
  type(unit_t), target :: pc = units_chain(7)
  type(unit_t), target :: ly = units_chain(8)
  type(unit_t), target :: AU = units_chain(9)
  type(unit_t), target :: Msun = units_chain(10)
  type(unit_t), target :: yr = units_chain(11)


  interface handle_pow
    procedure handle_pow_int
    procedure handle_pow_real
    procedure handle_pow_real8
  end interface handle_pow


  interface operator (.unitEqualsTo.)
    procedure units_unit_is_equal_to
  end interface operator (.unitEqualsTo.)

contains

  function units_parse (str) result (u)
    implicit none

    character(len=256), intent(in) :: str
    type(unit_t), pointer :: u

    character(len=8), dimension(32) :: arr


    u => null()

    arr = str_unit_tokenizer(str)

    u => units_term_parse (arr, 1)

  end function units_parse


  recursive function units_term_parse (arr, i) result (u)
    implicit none

    character(len=8), intent(in) :: arr(:)
    type(unit_t), pointer :: u

    type(unit_t), pointer :: unit_ptr
    integer, value :: i

    real(kind=8) :: exponent

    u => null()

    do while ( i <= size(arr) .and. .not. arr(i) == char(0) )
      select case ( trim(arr(i)) )

      case(")")
        if ( trim(arr(i+1)) .eq. "^" ) then
          read (arr(i+2), *) exponent
          u => u**exponent
        end if

        return

      case ("(")
        u => units_term_parse(arr, i+1)
        i = close_par_pos(arr, i) + 1
        if ( trim(arr(i)) .eq. "^" ) i = i + 2

      case ("^")
        read (arr(i+1), *) exponent
        u => u**exponent
        i = i + 2

      case ("*")
        if ( trim(arr(i+1)) .eq. "(" ) then
          u => u * units_term_parse(arr, i+2)
          i = close_par_pos(arr, i+1) + 1
          if ( trim(arr(i)) .eq. "^" ) i = i + 2
        else if ( trim(arr(i+2)) .eq. "^" ) then
          unit_ptr => units_parse_single(arr(i+1))
          read (arr(i+3), *) exponent
          unit_ptr => unit_ptr**exponent
          u => u * unit_ptr
          i = i + 4
        else
          unit_ptr => units_parse_single(arr(i+1))
          u => u * unit_ptr
          i = i + 2
        end if

      case ("/")
        if ( trim(arr(i+1)) .eq. "(" ) then
          u => u / units_term_parse(arr, i+2)
          i = close_par_pos(arr, i+1) + 1
          if ( trim(arr(i)) .eq. "^" ) i = i + 2
        else if ( trim(arr(i+2)) .eq. "^" ) then
          unit_ptr => units_parse_single(arr(i+1))
          read (arr(i+3), *) exponent
          unit_ptr => unit_ptr**exponent
          u => u / unit_ptr
          i = i + 4
        else
          unit_ptr => units_parse_single(arr(i+1))
          u => u / unit_ptr
          i = i + 2
        end if

      case default
        unit_ptr => units_parse_single(arr(i))
        u => unit_ptr
        i = i + 1

      end select
    end do
  end function units_term_parse


  function str_unit_tokenizer (str) result (arr)
    implicit none

    character(len=256), intent(in) :: str
    character(len=8), dimension(32) :: arr

    integer :: char_i, arr_i, i

    arr_i = 1
    char_i = 1
    arr(:)(:) = char(0)

    do i = 1, len_trim(str)
      if ( str(i:i) .eq. " " ) cycle

      if ( any( [ "^", "*", "/", "(", ")" ] .eq. str(i:i) ) ) then
        if ( .not. arr(arr_i) .eq. char(0) ) arr_i = arr_i + 1

        arr(arr_i) = str(i:i)
        arr_i = arr_i + 1
        char_i = 1
      else
        arr(arr_i)(char_i:char_i) = str(i:i)
        char_i = char_i + 1
      end if
    end do
  end function str_unit_tokenizer


  !> find close parenthesis
  !> @param[in] arr str extracted array
  !> #param[in] i index of the open parenthesis
  !> #param[in] j index of the clos parenthesis
  function close_par_pos (arr, i) result (pntr)
    implicit none

    character(len=8), dimension(32), intent(in) :: arr
    integer, intent(in) :: i
    integer :: pntr

    integer :: idx, j

    idx = i
    pntr = 0
    j = 0

    if ( .not. arr(idx) .eq. "(" ) return

    do while ( idx <= size(arr) )

      select case ( trim(arr(idx)) )
      case ("(")
        j = j + 1
      case (")")
        j = j - 1
        if ( j .eq. 0 ) then
          pntr = idx
          return
        end if
      end select

      idx = idx + 1
    end do
  end function close_par_pos


  !> @param[inout] u1
  !> @param[in] u2
  !> @param[in] op: operation character, * or /
  subroutine handle_mul_div ( u1, u2, op )
    implicit none


    type(unit_t), pointer :: u1, u2
    character, intent(in) :: op

    select case( trim(op) )
    case("*")
      u1 => u1 * u2
    case("/")
      u1 => u1 / u2
    end select
  end subroutine handle_mul_div


  subroutine handle_pow_int ( u, e )
    implicit none

    type(unit_t), pointer :: u
    integer :: e

    u => u**real(e, kind=8)
  end subroutine handle_pow_int


  subroutine handle_pow_real ( u, e )
    implicit none

    type(unit_t), pointer :: u
    real :: e

    u => u**real(e, kind=8)
  end subroutine handle_pow_real


  subroutine handle_pow_real8 ( u, e )
    implicit none

    type(unit_t), pointer :: u
    real(kind=8) :: e

    u => u**e
  end subroutine handle_pow_real8


  function units_unit_is_equal_to(u1, u2) result (comp)
    implicit none

    type(unit_t), pointer, intent(in) :: u1, u2
    logical :: comp

    comp = .false.

    if ( trim(u1%p()) .eq. trim(u2%p()) ) comp = .true.
  end function units_unit_is_equal_to


  function units_find (symb) result (u)
    implicit none

    character(len=*), intent(in) :: symb
    type(unit_t), pointer :: u

    integer :: i

    do i = 1, size(units_chain)
      if ( trim(symb) .eq. trim(units_chain(i)%symb) ) then
        if ( trim(units_chain(i)%symb) .eq. 'g' ) then
          u => mili * unit_hard_clone(units_chain(i))
        else
          u => unit_hard_clone(units_chain(i))
        end if
        return
      end if
    end do

    u => null()
  end function units_find


  function units_parse_single (symb) result (u)
    implicit none

    character(len=*), intent(in) :: symb
    type(unit_t), pointer :: u

    type(prefix_t) :: prfx
    type(unit_t), pointer :: unit => null()


    ! Check units first
    u => units_find(trim(symb))
    if ( associated(u) ) return


    ! Check combination of prefixes and units
    prfx = prefix_find(symb)
    unit => units_find( symb( len(trim(prfx%symb))+1 : ) )
    if ( .not. associated(unit) ) return

    u => prfx * unit

  end function units_parse_single

end module rhyme_units
