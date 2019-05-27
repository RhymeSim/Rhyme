submodule ( rhyme_chombo ) write_level_data_smod
contains
  module subroutine rhyme_chombo_write_level_data ( this, level )
    implicit none

    class ( chombo_t ), intent ( inout ) :: this
    type ( samr_level_t ), intent ( in ) :: level

    integer :: b, var, lb, ub, offset, length, dim1d, dims(3)
    real ( kind=8 ), allocatable :: d(:)
    integer, allocatable :: boxes(:,:)


    if ( .not. this%is_opened ) return

    offset = 1
    length = 0

    do b = 1, level%nboxes
      length = length + product( level%boxes(b)%dims ) * 5
    end do

    allocate ( d( length ) )
    allocate ( boxes( 6, level%nboxes ) )

    do b = 1, level%nboxes
      dims = level%boxes(b)%dims
      dim1d = product( dims )

      boxes( 1:3, b ) = level%boxes(b)%left_edge(:) - 1
      boxes( 4:6, b ) = level%boxes(b)%right_edge(:) - 1

      do var = hyid%rho, hyid%e_tot
        lb = offset + (var - 1) * dim1d
        ub = lb + dim1d - 1

        ! TODO: Instead of reshaping, we can directly copy the array using:
        ! use, intrinsic :: ISO_C_BINDING
        ! real, allocatable, target :: rank1_array(:)
        ! real, pointer :: rank3_array(:,:,:)
        ! call C_F_POINTER (C_LOC(rank1_array), rank3_array, [100,100,1000])
        d ( lb:ub ) = reshape ( &
          level%boxes(b)%hydro(1:dims(1),1:dims(2),1:dims(3))%u(var), &
          [ dim1d ] &
        )
      end do

      offset = offset + 5 * dim1d
    end do

    call this%write_1d_dataset( this%level_ids(level%level), "data:datatype=0", d )
    call this%write_table( &
      this%level_ids(level%level), "boxes", &
      [ "lo_i", "lo_j", "lo_k", "hi_i", "hi_j", "hi_k" ], &
      boxes &
    )

    deallocate ( d )
    deallocate ( boxes )
  end subroutine rhyme_chombo_write_level_data
end submodule write_level_data_smod
