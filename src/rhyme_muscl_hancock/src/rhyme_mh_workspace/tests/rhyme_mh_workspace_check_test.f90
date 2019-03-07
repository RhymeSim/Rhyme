logical function rhyme_mh_workspace_check_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none


  type ( mh_workspace_t ) :: ws
  integer :: l, b, lb(3), ub(3), lbws(4), ubws(4), dims(3)


  call rhyme_mh_workspace_factory_init


  call ws%init( samr, log )

  do l = 0, samr%nlevels
    do b = 1, samr%levels(l)%nboxes
      call ws%check ( samr%levels(l)%boxes(b) )

      lb = lbound ( samr%levels(l)%boxes(b)%hydro )
      ub = ubound ( samr%levels(l)%boxes(b)%hydro )

      dims = ub - lb + 1

      lbws = lbound ( ws%levels(l)%boxes(b)%UL )
      ubws = ubound ( ws%levels(l)%boxes(b)%UL )

      failed = &
      .not. allocated ( ws%levels(l)%boxes(b)%UL ) &
      .or. .not. allocated ( ws%levels(l)%boxes(b)%UR ) &
      .or. .not. allocated ( ws%levels(l)%boxes(b)%FR ) &
      .or. size ( ws%levels(l)%boxes(b)%UL ) .ne. product ( dims ) * 3 &
      .or. size ( ws%levels(l)%boxes(b)%UR ) .ne. product ( dims ) * 3 &
      .or. size ( ws%levels(l)%boxes(b)%FR ) .ne. product ( dims ) * 3 &
      .or. any ( lb .ne. lbws(:3) ) &
      .or. any ( ub .ne. ubws(:3) )
    end do
  end do
end function rhyme_mh_workspace_check_test
