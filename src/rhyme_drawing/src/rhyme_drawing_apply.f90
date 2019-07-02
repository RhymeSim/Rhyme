submodule ( rhyme_drawing ) rhyme_drawing_apply_submodule
contains
  module subroutine rhyme_drawing_apply ( draw, samr, logger )
    implicit none

    type ( drawing_t ), intent ( inout ) :: draw
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: logger

    type ( shape_t ), pointer :: shape

    call logger%begin_section( 'drawing' )

    ! No action for transparent
    if ( draw%type .eq. drid%uniform_canvas ) then
      call rhyme_drawing_uniform_canvas( samr, draw%canvas )
      call logger%log( 'Using uniform canvas', 'color', '=', draw%canvas )
    else if ( draw%type .eq. drid%transparent_canvas ) then
      call logger%log( 'Using transparent canvas' )
    else
      call logger%err( 'Unknown canvas type', 'canvas (type)', '=', [ draw%type ] )
    end if

    shape => draw%shapes

    do while ( associated( shape ) )
      select case ( shape%type )
      case ( drid%cuboid )
        if ( shape%fill%type .eq. drid%uniform) then
          call logger%log( 'uniform cuboid...' )
          call rhyme_drawing_uniform_cuboid( samr, shape )
        end if

      case ( drid%sphere )
        if ( shape%fill%type .eq. drid%uniform ) then
          call logger%log( 'uniform sphere...' )
          call rhyme_drawing_uniform_sphere( samr, shape )
        end if

#if NDIM > 1
      case ( drid%prism )
        if ( shape%fill%type .eq. drid%uniform) then
          call logger%log( 'uniform prism...' )
          call rhyme_drawing_uniform_prism( samr, shape )
        end if

      case ( drid%smoothed_slab_2d )
        call logger%log( 'smoothed slab (2d)...')
        call rhyme_drawing_smoothed_slab_2d( samr, shape, logger )
#endif

      end select

      shape => shape%next
    end do


    call logger%log( 'perturbations...' )
    call rhyme_drawing_apply_perturbations( samr, draw%perturbs, logger )

    call logger%end_section
  end subroutine rhyme_drawing_apply
end submodule rhyme_drawing_apply_submodule
