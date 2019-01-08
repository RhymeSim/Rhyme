module rhyme_prefix

  implicit none


  type prefix_t
    character(len=8) :: symb
    integer :: base_10
  end type prefix_t


  type(prefix_t), dimension(-24:24), parameter :: prfx_si = (/ &
    prefix_t("y", -24), prefix_t("", -23), prefix_t("", -22), &
    prefix_t("z", -21), prefix_t("", -20), prefix_t("", -19), &
    prefix_t("a", -18), prefix_t("", -17), prefix_t("", -16), &
    prefix_t("f", -15), prefix_t("", -14), prefix_t("", -13), &
    prefix_t("p", -12), prefix_t("", -11), prefix_t("", -10), &
    prefix_t("n", -9), prefix_t("", -8), prefix_t("", -7), &
    prefix_t("mu", -6), prefix_t("", -5), prefix_t("", -4), &
    prefix_t("m", -3), prefix_t("c", -2), prefix_t("d", -1), &
    prefix_t("", 0), &
    prefix_t("da", 1), prefix_t("h", 2), prefix_t("k", 3), &
    prefix_t("", 4), prefix_t("", 5), prefix_t("M", 6), &
    prefix_t("", 7), prefix_t("", 8), prefix_t("G", 9), &
    prefix_t("", 10), prefix_t("", 11), prefix_t("T", 12), &
    prefix_t("", 13), prefix_t("", 14), prefix_t("P", 15), &
    prefix_t("", 16), prefix_t("", 17), prefix_t("E", 18), &
    prefix_t("", 19), prefix_t("", 20), prefix_t("Z", 21), &
    prefix_t("", 22), prefix_t("", 23), prefix_t("Y", 24) &
  /)


  type(prefix_t), parameter :: yotta = prfx_si(24)
  type(prefix_t), parameter :: zetta = prfx_si(21)
  type(prefix_t), parameter :: exa = prfx_si(18)
  type(prefix_t), parameter :: peta = prfx_si(15)
  type(prefix_t), parameter :: tera = prfx_si(12)
  type(prefix_t), parameter :: giga = prfx_si(9)
  type(prefix_t), parameter :: mega = prfx_si(6)
  type(prefix_t), parameter :: kilo = prfx_si(3)
  type(prefix_t), parameter :: hecto = prfx_si(2)
  type(prefix_t), parameter :: deca = prfx_si(1)
  type(prefix_t), parameter :: one = prfx_si(0)
  type(prefix_t), parameter :: deci = prfx_si(-1)
  type(prefix_t), parameter :: centi = prfx_si(-2)
  type(prefix_t), parameter :: mili = prfx_si(-3)
  type(prefix_t), parameter :: micro = prfx_si(-6)
  type(prefix_t), parameter :: nano = prfx_si(-9)
  type(prefix_t), parameter :: pico = prfx_si(-12)
  type(prefix_t), parameter :: femto = prfx_si(-15)
  type(prefix_t), parameter :: atto = prfx_si(-18)
  type(prefix_t), parameter :: zepto = prfx_si(-21)
  type(prefix_t), parameter :: yocto = prfx_si(-24)


  interface operator (*)
    procedure prefix_mul
  end interface operator (*)

contains

  function prefix_mul (p1, p2) result(p)
    implicit none

    type(prefix_t), target, intent(in) :: p1, p2
    type(prefix_t) :: p

    integer :: i

    i = p1%base_10 + p2%base_10

    if ( i < -24 .or. i > 24 ) then
      p = prefix_t("", i)
    else
      p = prfx_si(i)
    end if
  end function prefix_mul


  function prefix_find (symb) result (p)
    implicit none

    character(len=*), intent(in) :: symb
    type(prefix_t) :: p

    integer :: i, lb(1), ub(1)

    lb = lbound ( prfx_si )
    ub = ubound ( prfx_si )

    do i = lb(1), ub(1)
      if ( len(trim(prfx_si(i)%symb)) == 0 ) cycle

      if ( trim(prfx_si(i)%symb) .eq. trim(symb( 1:len(trim(prfx_si(i)%symb))) ) ) then
        if ( prfx_si(i)%symb .eq. "d" .and. symb(2:2) .eq. "a" ) cycle ! Handle the "da" case

        p = prfx_si(i)
        return
      end if
    end do

    p = one
  end function prefix_find

end module rhyme_prefix
