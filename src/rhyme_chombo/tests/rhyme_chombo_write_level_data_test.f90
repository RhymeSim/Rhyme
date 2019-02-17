logical function rhyme_chombo_write_level_data_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  ! rhyme_chombo variables
  type ( chombo_t ) :: ch

  ! Chombo filename
  character ( len=1024 ) :: filename = " "

  ! variables
  integer :: l, b, length, bdims(3), offset, lb, ub
  character ( len=16 ) :: level_name
  character ( len=32 ) :: level_data_name
  real ( kind=8 ), allocatable :: data(:), expected_data(:)

  call rhyme_chombo_factory_init

  ! Crete chombo file
  ch%nickname = "rhyme_chombo_write_level_data"
  call ch%filename_generator ( samr%levels(0)%iteration, filename )
  call ch%create ( filename )


  ! Prepare level groups
  do l = 0, samr%nlevels - 1
    write ( level_name, '(A7,I1)') "/level_", l
    call ch%create_group ( level_name, ch%level_ids(l) )
    call ch%write_group_1d_array_attr ( level_name, "dx", samr%levels(l)%dx )
    call ch%write_group_attr ( level_name, "ref_ratio", samr%levels(l)%refine_factor )
  end do

  do l = 0, samr%nlevels - 1
    call ch%write_level_data ( samr%levels(l) )
  end do

  call ch%close

  ! call ch%open ( filename )
  !
  ! do l = 0, samr%nlevels - 1
  !   write ( level_data_name, '(A7,I1,A)') "/level_", l, "/data:datatype=0"
  !
  !   length = 0
  !   do b = 1, samr%levels(l)%nboxes
  !     length = length + product ( samr%levels(l)%boxes(b)%dims )
  !   end do
  !
  !   allocate ( data( 5 * length ) )
  !   allocate ( expected_data( 5 * length ) )
  !
  !   call ch%read_1d_dataset ( level_data_name, data )
  !
  !   offset = 1
  !   do b = 1, samr%levels(l)%nboxes
  !     bdims = samr%levels(l)%boxes(b)%dims
  !
  !     ! Rho
  !     lb = offset
  !     ub = lb + product( bdims ) - 1
  !
  !     expected_data(lb:ub) = reshape( &
  !       samr%levels(l)%boxes(b)%hydro(1:bdims(1),1:bdims(2),1:bdims(3))%u(hyid%rho), &
  !       [ product( bdims ) ] &
  !     )
  !
  !     failed = any ( abs( data(lb:ub) - expected_data(lb:ub) ) > epsilon(0.d0) )
  !     if ( failed ) return
  !
  !     ! E_tot
  !     lb = lb + (hyid%e_tot - 1) * product( bdims )
  !     ub = lb + product( bdims ) - 1
  !
  !     expected_data(lb:ub) = reshape( &
  !       samr%levels(l)%boxes(b)%hydro(1:bdims(1),1:bdims(2),1:bdims(3))%u(hyid%e_tot), &
  !       [ product( bdims ) ] &
  !     )
  !
  !     failed = any ( abs( data(lb:ub) - expected_data(lb:ub) ) > epsilon(0.d0) )
  !     if ( failed ) return
  !
  !     offset = offset + 5 * product( bdims )
  !   end do
  !
  !   deallocate ( data )
  !   deallocate ( expected_data )
  !
  !   ! TODO: Add test for boxes
  ! end do
    failed = .true.
end function rhyme_chombo_write_level_data_test