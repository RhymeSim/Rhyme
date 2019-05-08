submodule ( rhyme_initial_condition ) rhyme_ic_load_rhyme_smod
contains
  module subroutine rhyme_initial_condition_load_rhyme ( ic, samr, logger )
    implicit none

    type ( initial_condition_t ), intent ( in ) :: ic
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: logger

    integer, parameter :: ncomp = 5

    type ( chombo_t ) :: ch
    integer :: l, b, ofs
    integer :: nboxes, lboxes
    integer :: bdims(3), ub(3), blen
    character ( len=16 ) :: level_name
    integer, allocatable :: boxes(:,:)
    real ( kind=8 ), allocatable :: data(:)

    call ch%open( ic%snapshot_path )

    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I0)') "/level_", l

      allocate( samr%levels(l)%boxes( samr%levels(l)%max_nboxes ) )

      nboxes = ch%get_table_size( trim(level_name)//'/boxes' )
      allocate( boxes( 6, nboxes ) )

      call ch%read_table( trim(level_name), 'boxes', icid%boxes_headers, boxes )

      if ( nboxes > samr%levels(l)%max_nboxes ) then
        call logger%err( 'Number of boxes is less than maximum available', &
          nboxes, '>', [ samr%levels(l)%max_nboxes ] )
        return
      end if

      lboxes = sum( [ (product(boxes(4:6, b) + 1), b=1, nboxes ) ] )

      ! Reading data dataset
      allocate( data( ncomp * lboxes ) )
      call ch%read_1d_dataset( trim(level_name)//'/data:datatype=0', data )

      ofs = 0
      do b = 1, nboxes
        samr%levels(l)%boxes(b)%level = l
        samr%levels(l)%boxes(b)%number = b

        bdims = boxes(4:6, b) - boxes(1:3, b) + 1
        blen = product( bdims )
        ub = bdims

        call samr%init_box( l, b, bdims, boxes(1:3, b) + 1, boxes(4:6, b) + 1 )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho) = &
        reshape( data( ofs+0*blen+1:ofs+1*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_u) = &
        reshape( data( ofs+1*blen+1:ofs+2*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_v) = &
        reshape( data( ofs+2*blen+1:ofs+3*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%rho_w) = &
        reshape( data( ofs+3*blen+1:ofs+4*blen ), bdims )

        samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(hyid%e_tot) = &
        reshape( data( ofs+4*blen+1:ofs+5*blen ), bdims )

        ofs = ofs + ncomp * blen
      end do

      deallocate( data )
      deallocate( boxes )
    end do

    call ch%close
  end subroutine rhyme_initial_condition_load_rhyme
end submodule rhyme_ic_load_rhyme_smod
