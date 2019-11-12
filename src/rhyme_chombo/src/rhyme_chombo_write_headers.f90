submodule ( rhyme_chombo ) write_headers_smod
contains
  module subroutine rhyme_chombo_write_headers ( chombo, physics, samr )
    implicit none

    type ( chombo_t ), intent ( inout ) :: chombo
    type ( physics_t ), intent ( in ) :: physics
    type ( samr_t ), intent ( in ) :: samr

    integer :: l, i, boxes_shape(6)
    character ( len=16 ) :: level_name

    if ( .not. chombo%is_opened ) return

    do l = 0, samr%nlevels - 1
      write( level_name, '(A7,I1)' ) '/level_', l
      call rhyme_hdf5_util_create_group( chombo%file, level_name, chombo%level_ids(l) )
      call rhyme_hdf5_util_write_group_1d_array_attr( chombo%file, level_name, 'dx', &
        reshape( samr%levels(l)%dx, shape=[3], pad=[0.d0] ))
      call rhyme_hdf5_util_write_group_attr( chombo%file, level_name, &
        'ref_ratio', samr%levels(l)%refine_factor )
    end do

    boxes_shape = 0
    boxes_shape( 4:6 ) = reshape( samr%base_grid - 1, shape=[3], pad=[1] )

    call rhyme_hdf5_util_write_group_comp_1d_array_attr( chombo%file, '/level_0', &
      'prob_domain', [ 'lo_i', 'lo_j', 'lo_k', 'hi_i', 'hi_j', 'hi_k' ], &
      boxes_shape )

    call rhyme_hdf5_util_write_group_1d_array_attr( chombo%file, '/', 'ProblemDomain', &
      reshape( samr%base_grid, shape=[3], pad=[1] ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/', 'num_levels', samr%nlevels )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/', 'num_components', NCMP )

    do i = 1, NCMP
      call rhyme_hdf5_util_write_group_attr( chombo%file, '/', &
        chombo%cmp_labels( 1, i ), chombo%cmp_labels( 2, i ) )
    end do

    call rhyme_hdf5_util_write_group_attr( chombo%file, '/', 'iteration', samr%levels(0)%iteration )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/', 'time', samr%levels(0)%t )


    call rhyme_hdf5_util_create_group( chombo%file, '/Chombo_global', chombo%chombo_global_id )

    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', 'SpaceDim', 3 )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'length_unit', trim( .printchain. physics%length ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'density_unit', trim( .printchain. physics%rho ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'time_unit', trim( .printchain. physics%time ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'velocity_unit', trim( .printchain. physics%velocity ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'pressure_unit', trim( .printchain. physics%pressure ) )
    call rhyme_hdf5_util_write_group_attr( chombo%file, '/Chombo_global', &
      'temperature_unit', trim( .printchain. physics%temperature ) )
  end subroutine rhyme_chombo_write_headers
end submodule write_headers_smod
