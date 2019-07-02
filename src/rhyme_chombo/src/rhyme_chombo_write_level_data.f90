submodule ( rhyme_chombo ) write_level_data_smod
contains
  module subroutine rhyme_chombo_write_level_data ( chombo, level )
    implicit none

    type ( chombo_t ), intent ( inout ) :: chombo
    type ( samr_level_t ), intent ( in ) :: level

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J ,1:dims(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J ,1:dims(2)
#define RANGE_K ,1:dims(3)
#endif

    integer :: b, var, lb, ub, offset, length, porduct_dims, dims( NDIM )

    if ( .not. chombo%is_opened ) return

    length = 0
    do b = 1, level%nboxes
      length = length + product( level%boxes(b)%dims ) * NCMP
    end do

    if ( size( chws%data ) < length ) then
      deallocate( chws%data )
      allocate( chws%data( length ) )
    end if

    if ( size( chws%boxes, dim=2 ) < level%nboxes ) then
      deallocate( chws%boxes )
      allocate( chws%boxes( 6, level%max_nboxes ) )
    end if

    offset = 1

    do b = 1, level%nboxes
      dims = level%boxes(b)%dims
      porduct_dims = product( dims )

      chws%boxes( 1:3, b ) = reshape( level%boxes(b)%left_edge - 1, shape=[3], pad=[0] )
      chws%boxes( 4:6, b ) = reshape( level%boxes(b)%right_edge - 1, shape=[3], pad=[0] )

      do var = 1, NCMP
        lb = offset + (var - 1) * porduct_dims
        ub = lb + porduct_dims - 1

        ! TODO: Instead of reshaping, we can directly copy the array using:
        ! use, intrinsic :: ISO_C_BINDING
        ! real, allocatable, target :: rank1_array(:)
        ! real, pointer :: rank3_array(:,:,:)
        ! call C_F_POINTER (C_LOC(rank1_array), rank3_array, [100,100,1000])
        chws%data( lb:ub ) = reshape( &
          level%boxes(b)%cells(1:dims(1) RANGE_J RANGE_K, var), [ porduct_dims ] )
      end do

      offset = offset + NCMP * porduct_dims
    end do

    call rhyme_hdf5_util_write_1d_dataset( chombo%file, chombo%level_ids(level%level), &
      "data:datatype=0", chws%data( 1:length ) )
    call rhyme_hdf5_util_write_table( chombo%file, chombo%level_ids(level%level), &
      "boxes", chid%boxes_headers, chws%boxes( 1:6, 1:level%nboxes ) )
  end subroutine rhyme_chombo_write_level_data
end submodule write_level_data_smod
