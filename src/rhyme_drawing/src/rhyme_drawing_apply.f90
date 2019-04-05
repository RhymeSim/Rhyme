submodule ( rhyme_drawing ) rhyme_drawing_apply_submodule
contains
  module subroutine rhyme_drawing_apply ( cfg, ig, samr, log )
    implicit none

    class ( drawing_t ), intent ( inout ) :: cfg
    type ( ideal_gas_t ), intent( in ) :: ig
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    type ( shape_t ), pointer :: shape

    call log%set_sub_section( 'drawing' )

    ! No action for transparent
    if ( cfg%type .eq. drid%uniform_canvas ) then
      call rhyme_drawing_uniform_canvas( samr, ig, cfg%canvas )
      call log%log( 'Using uniform canvas', 'color', '=', cfg%canvas%w )
    else if ( cfg%type .eq. drid%transparent_canvas ) then
      call log%log( 'Using transparent canvas' )
    else
      call log%err( 'Unknown canvas type', 'canvas (type)', '=', [ cfg%type ] )
    end if

    shape => cfg%shapes

    do while ( associated( shape ) )
      select case ( shape%type )
      case ( drid%cuboid )
        if ( shape%fill%type .eq. drid%uniform) then
          call log%log( 'uniform cuboid...' )
          call rhyme_drawing_uniform_cuboid( samr, ig, shape )
        end if

      case ( drid%sphere )
        if ( shape%fill%type .eq. drid%uniform ) then
          call log%log( 'uniform sphere...' )
          call rhyme_drawing_uniform_sphere( samr, ig, shape )
        end if

      case ( drid%prism )
        if ( shape%fill%type .eq. drid%uniform) then
          call log%log( 'uniform prism...' )
          call rhyme_drawing_uniform_prism( samr, ig, shape )
        end if

      case ( drid%smoothed_slab_2d )
        call log%log( 'smoothed slab (2d)...')
        call rhyme_drawing_smoothed_slab_2d( samr, ig, shape, log )

      end select

      shape => shape%next
    end do


    call log%log( 'perturbations...' )
    call rhyme_drawing_apply_perturbations( samr, ig, cfg%perturbs, log )

    call log%set_sub_section('')
  end subroutine rhyme_drawing_apply
end submodule rhyme_drawing_apply_submodule
