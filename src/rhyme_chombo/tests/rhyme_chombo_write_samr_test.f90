logical function rhyme_chombo_write_samr_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  ! rhyme_chombo variables
  type ( chombo_t ) :: ch

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_samr"
  character ( len=1024 ) :: filename, level_name

  logical :: exists
  integer :: l, hdferr
  integer ( kind=hid_t ) :: file_id


  call rhyme_chombo_factory_init

  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration
  call ch%filename_generator ( filename )

  call ch%write_samr ( samr )

  call h5open_f ( hdferr )
  call h5fopen_f ( filename, H5F_ACC_RDONLY_F, file_id, hdferr )

  do l = 0, samr%nlevels - 1
    write ( level_name, '(A7,I1)') "/level_", l

    call h5lexists_f ( file_id, trim(level_name)//"/boxes", exists, hdferr )
    failed = .not. exists
    if ( failed ) return

    call h5lexists_f ( file_id, trim(level_name)//"/data:datatype=0", exists, hdferr )
    failed = .not. exists
    if ( failed ) return
  end do

  call h5fclose_f ( file_id, hdferr )
  call h5close_f ( hdferr )

  failed = &
  any ( ch%level_ids .ne. chid%unset ) &
  .or. ch%chombo_global_id .ne. chid%unset &
  .or. ch%is_opened
end function rhyme_chombo_write_samr_test
