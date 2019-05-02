submodule ( rhyme_string ) rhyme_string_to_string_submodule
contains
  pure module function rhyme_string_array_to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input(:)
    character ( len=2048 ) :: str

    character ( len=32 ) :: ch_arr( size(input) )
    integer :: i

    str = ''
    ch_arr = rhyme_string_to_string( input )

    do i = 1, size(input)
      str = trim( adjustl(str) ) // ' ' // trim( adjustl(ch_arr(i)) )
    end do

    str = '[ ' // trim( adjustl(str) ) // ' ]'
  end function rhyme_string_array_to_string


  pure elemental module function rhyme_string_to_string ( input ) result ( str )
    use, intrinsic :: ieee_arithmetic

    implicit none

    class (*), intent ( in ) :: input
    character ( len=32 ) :: str

    if ( rhyme_string_is_nan( input ) ) then
        str = 'NaN'
    else
      select type ( inp => input )
      type is ( integer )
        write ( str, strcnst%int_fmt ) inp
      type is ( real( kind=4 ) )
        write ( str, strcnst%real_fmt ) inp
      type is ( real( kind=8 ) )
        write ( str, strcnst%double_fmt ) inp
      type is ( character (*) )
        str = "'" // trim( adjustl(inp) ) // "'"
      type is ( logical )
        if ( inp ) then
          str = '.true.'
        else
          str = '.false.'
        end if
      class default
        str = unknown_type_str
      end select
    end if
  end function rhyme_string_to_string
end submodule rhyme_string_to_string_submodule
