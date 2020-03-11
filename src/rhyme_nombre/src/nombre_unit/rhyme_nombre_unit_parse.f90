submodule(rhyme_nombre_unit) parse_smod
contains
module function rhyme_nombre_unit_parse(str) result(duc)
   implicit none

   character(len=*), intent(in) :: str
   type(nombre_unit_t), pointer :: duc

   character(len=8), dimension(64) :: str_arr

   duc => null()

   str_arr = rhyme_nombre_unit_tokenize(str)
   duc => rhyme_nombre_unit_parse_term(str_arr, 1)

   if (associated(duc)) duc => .head.duc
end function rhyme_nombre_unit_parse

recursive function rhyme_nombre_unit_parse_term(str_arr, i) result(duc)
   implicit none

   character(len=8), intent(in) :: str_arr(:)
   integer, value :: i
   type(nombre_unit_t), pointer :: duc

   real(kind=8) :: ex

   do while (i <= size(str_arr) .and. .not. str_arr(i) == char(0))
      select case (str_arr(i))
      case (')') ! Only for the ) at the end of the string
         if (str_arr(i + 1) .eq. '^') then
            read (str_arr(i + 2), *) ex
            duc => duc**ex
         end if
         return

      case ('(') ! Only for the ( at the beginning of the string
         duc => rhyme_nombre_unit_parse_term(str_arr, i + 1)
         i = rhyme_nombre_unit_close_par_pos(str_arr, i) + 1
         if (str_arr(i) .eq. '^') i = i + 2

      case ('^')
         read (str_arr(i + 1), *) ex
         duc => duc**ex
         i = i + 2

      case ('*')
         if (str_arr(i + 2) .eq. '^') then
            read (str_arr(i + 3), *) ex
            duc => duc*rhyme_nombre_unit_parse_unit(str_arr(i + 1))**ex
            i = i + 4
         else
            duc => duc*rhyme_nombre_unit_parse_unit(str_arr(i + 1))
            i = i + 2
         end if

      case ('/')
         if (str_arr(i + 2) .eq. '^') then
            read (str_arr(i + 3), *) ex
            duc => duc/rhyme_nombre_unit_parse_unit(str_arr(i + 1))**ex
            i = i + 4
         else
            duc => duc/rhyme_nombre_unit_parse_unit(str_arr(i + 1))
            i = i + 2
         end if

      case default ! For the first unit
         duc => rhyme_nombre_unit_parse_unit(str_arr(i))
         i = i + 1

      end select
   end do
end function rhyme_nombre_unit_parse_term

function rhyme_nombre_unit_tokenize(str) result(arr)
   implicit none

   character(len=*), intent(in) :: str
   character(len=8), dimension(64) :: arr

   integer :: i, chr_i, arr_i

   arr_i = 1
   chr_i = 1

   arr = char(0)

   do i = 1, len_trim(str)
      if (str(i:i) .eq. ' ') cycle

      if (any(['^', '*', '/', '(', ')'] .eq. str(i:i))) then
         if (arr(arr_i) .ne. char(0)) arr_i = arr_i + 1

         arr(arr_i) = str(i:i)

         arr_i = arr_i + 1
         chr_i = 1
      else
         arr(arr_i) (chr_i:chr_i) = str(i:i)
         chr_i = chr_i + 1
      end if
   end do
end function rhyme_nombre_unit_tokenize

function rhyme_nombre_unit_close_par_pos(arr, open_par_pos) result(loc)
   implicit none

   character(len=8), dimension(64), intent(in) :: arr
   integer, intent(in) :: open_par_pos
   integer :: loc

   integer :: i, n_open_pars

   loc = 0

   i = open_par_pos
   n_open_pars = 0

   if (.not. arr(i) .eq. "(") return

   do while (i <= size(arr))
      select case (trim(arr(i)))
      case ("(")
         n_open_pars = n_open_pars + 1
      case (")")
         n_open_pars = n_open_pars - 1

         if (n_open_pars .eq. 0) then
            loc = i
            return
         end if
      end select

      i = i + 1
   end do
end function rhyme_nombre_unit_close_par_pos

function rhyme_nombre_unit_parse_unit(str) result(du)
   implicit none

   character(len=*), intent(in) :: str
   type(nombre_unit_t), pointer :: du

   type(nombre_base_unit_t), pointer :: u_tmp => null()
   type(nombre_unit_t), pointer :: du_tmp => null()

   du => null()

   u_tmp => rhyme_nombre_base_unit_parse(str)
   if (associated(u_tmp)) then
      du => 1d0*(.clone.u_tmp)
      return
   end if

   du_tmp => rhyme_nombre_derived_unit_parse(str)
   if (associated(du_tmp)) then
      du => .clone.du_tmp
      return
   end if
end function rhyme_nombre_unit_parse_unit
end submodule parse_smod
