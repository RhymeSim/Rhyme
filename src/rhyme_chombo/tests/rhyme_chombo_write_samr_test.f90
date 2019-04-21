logical function rhyme_chombo_write_samr_test () result ( failed )
  use rhyme_chombo_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  ! rhyme_chombo variables
  type ( chombo_t ) :: ch

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_samr"
  character ( len=1024 ) :: filename, level_name

  logical :: exists
  integer :: l, hdferr
  integer ( kind=hid_t ) :: file_id

  ch_tester = .describe. "chombo write_samr"

  call rhyme_chombo_factory_init

  ch%nickname = nickname
  ch%iteration = chombo_fac_samr%levels(0)%iteration
  call ch%filename_generator ( filename )

  call ch%write_samr ( chombo_fac_samr )

  call h5open_f ( hdferr )
  call h5fopen_f ( filename, H5F_ACC_RDONLY_F, file_id, hdferr )

  do l = 0, chombo_fac_samr%nlevels - 1
    write ( level_name, '(A7,I1)') "/level_", l

    call h5lexists_f ( file_id, trim(level_name)//"/boxes", exists, hdferr )
    call ch_tester%expect( exists .toBe. .true. .hint. trim(level_name)//"/boxes" )

    call h5lexists_f ( file_id, trim(level_name)//"/data:datatype=0", exists, hdferr )
    call ch_tester%expect( exists .toBe. .true. .hint. trim(level_name)//"/data:datatype=0" )
  end do

  call h5fclose_f ( file_id, hdferr )
  call h5close_f ( hdferr )

  call ch_tester%expect( int( ch%level_ids ) .toBe. chid%unset )
  call ch_tester%expect( int( ch%chombo_global_id ) .toBe. chid%unset )
  call ch_tester%expect( ch%is_opened .toBe. .false. )

  failed = ch_tester%failed()
end function rhyme_chombo_write_samr_test
