submodule ( rhyme_param_parser ) rhyme_param_parser_read_array_submodule
contains
  module subroutine rhyme_param_parser_read_array ( this, term, var, logger, switch )
    use rhyme_string

    implicit none

    class ( config_t ), intent ( inout ) :: this
    type ( config_term_t ), intent ( in ) :: term
    class (*), intent ( inout ) :: var(:)
    type ( logger_t ), intent ( inout ) :: logger
    type ( config_switch_t ), optional, intent ( in ) :: switch

    character ( len=1024 ) :: key, op, str(10), switch_str( size( var ) )
    integer :: i, j, occur, ios, lb, ub

    lb = lbound( var, dim=1 )
    ub = ubound( var, dim=1 )

    occur = 0

    open( 1234, file=this%path, action='read', form="formatted" )

    do
      read ( 1234, *, iostat=ios ) key
      if ( ios .ne. 0 ) exit

      key = adjustl( key )
      if ( key(1:1) .eq. '#' ) cycle

      if ( trim( key ) .eq. trim( term%key ) ) then
        occur = occur + 1

        if ( occur < term%occurence ) cycle

        backspace(1234)

        if ( present( switch ) ) then
          read( 1234, * ) key, op, str( 1:term%location-1 ), switch_str( 1:size(var) )

          select type ( v => var )
          type is ( integer )
            do j = 1, size( var )
              do i = 1, switch%len
                if ( switch_str(j) .eq. switch%keys(i) ) then
                  v(j) = switch%values( i )
                end if
              end do
            end do

            call logger%log( '', term%key, '=>', [ .toString. switch_str( 1:size(var) ) ] )

          class default
            read( 1234, * )
            call logger%err( 'Reading '//trim(term%key)//' with switch into non integer' )
          end select

        else
          select type ( v => var )
          type is ( integer )
            read( 1234, * ) key, op, str( 1:term%location-1 ), v( lb:ub )
            call logger%log( term%key, term%hint, '=', v )
          type is ( real( kind=4 ) )
            read( 1234, * ) key, op, str( 1:term%location-1 ), v( lb:ub )
            call logger%log( term%key, term%hint, '=', v )
          type is ( real( kind=8 ) )
            read( 1234, * ) key, op, str( 1:term%location-1 ), v( lb:ub )
            call logger%log( term%key, term%hint, '=', v )
          type is ( character(*) )
            read( 1234, * ) key, op, str( 1:term%location-1 ), v( lb:ub )
            call logger%log( term%key, term%hint, '=', v )
          type is ( logical )
            read( 1234, * ) key, op, str( 1:term%location-1 ), v( lb:ub )
            call logger%log( term%key, term%hint, '=', v )
            class default
            read( 1234, * )
            call logger%err( 'Unknonw type', 'key', '=', [ term%key ] )
          end select
        end if

        exit
      end if
    end do

    close( 1234 )
  end subroutine rhyme_param_parser_read_array
end submodule rhyme_param_parser_read_array_submodule
