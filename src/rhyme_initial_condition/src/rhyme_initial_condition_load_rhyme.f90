submodule ( rhyme_initial_condition ) rhyme_ic_load_rhyme_smod
contains
  module subroutine rhyme_initial_condition_load_rhyme ( ic, samr, logger )
    implicit none

    type ( initial_condition_t ), intent ( in ) :: ic
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: logger

    type ( chombo_t ) :: ch
    integer :: l, b, ofs
    integer :: nboxes, lboxes
    integer :: bdims( NDIM ), ub( NDIM ), blen
    character ( len=16 ) :: level_name
    integer, allocatable :: boxes(:,:)
    real ( kind=8 ), allocatable :: data(:)

    call rhyme_hdf5_util_open( ch%file, ic%snapshot_path )

    do l = 0, samr%nlevels - 1
      write( level_name, '(A7,I0)') "/level_", l

      allocate( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      nboxes = rhyme_hdf5_util_get_table_size( ch%file, trim(level_name)//'/boxes' )
      allocate( boxes( 6, nboxes ) )

      call rhyme_hdf5_util_read_table( ch%file, trim(level_name), 'boxes', chid%boxes_headers, boxes )

      if ( nboxes > samr%levels(l)%max_nboxes ) then
        call logger%err( 'Number of boxes is less than maximum available', &
          nboxes, '>', [ samr%levels(l)%max_nboxes ] )
        return
      end if

      lboxes = sum( [ (product(boxes(4:4+NDIM-1, b) + 1), b=1, nboxes ) ] )

      ! Reading data dataset
      allocate( data( NCMP * lboxes ) )
      call rhyme_hdf5_util_read_1d_dataset( ch%file, trim(level_name)//'/data:datatype=0', data )

      ofs = 0
      do b = 1, nboxes
        samr%levels(l)%boxes(b)%level = l
        samr%levels(l)%boxes(b)%number = b

        bdims = boxes(4:4+NDIM-1, b) - boxes(1:NDIM , b) + 1
        blen = product( bdims )
        ub = bdims

        call samr%init_box( l, b, bdims, boxes(1:NDIM , b) + 1, boxes(4:4+NDIM-1, b) + 1 )

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J ,1:ub(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J ,1:ub(2)
#define RANGE_K ,1:ub(3)
#endif

        samr%levels(l)%boxes(b)%cells( 1:ub(1) RANGE_J RANGE_K, cid%rho ) = &
        reshape( data( ofs+0*blen+1:ofs+1*blen ), bdims )

        samr%levels(l)%boxes(b)%cells( 1:ub(1) RANGE_J RANGE_K, cid%rho_u ) = &
        reshape( data( ofs+1*blen+1:ofs+2*blen ), bdims )

#if NDIM > 1
        samr%levels(l)%boxes(b)%cells( 1:ub(1) RANGE_J RANGE_K, cid%rho_v ) = &
        reshape( data( ofs+2*blen+1:ofs+3*blen ), bdims )
#endif

#if NDIM > 2
        samr%levels(l)%boxes(b)%cells( 1:ub(1) RANGE_J RANGE_K, cid%rho_w ) = &
        reshape( data( ofs+3*blen+1:ofs+4*blen ), bdims )
#endif

        samr%levels(l)%boxes(b)%cells( 1:ub(1) RANGE_J RANGE_K, cid%e_tot ) = &
        reshape( data( ofs+(NDIM+1)*blen+1:ofs+(NDIM+2)*blen ), bdims )

        ofs = ofs + NCMP * blen
      end do

      deallocate( data )
      deallocate( boxes )
    end do

    call rhyme_hdf5_util_close( ch%file )
  end subroutine rhyme_initial_condition_load_rhyme
end submodule rhyme_ic_load_rhyme_smod
