logical function rhyme_mh_workspace_check_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none


  type ( mh_workspace_t ) :: ws
  integer :: l, b, lb(3), ub(3), lbws(5), ubws(5), dims(3)


  call rhyme_mh_workspace_factory_init


  call ws%init ( samr )

  do l = 0, samr%nlevels
    do b = 1, samr%levels(l)%nboxes
      call ws%check ( l, b, samr%levels(l)%boxes(b) )

      lb = lbound ( samr%levels(l)%boxes(b)%hydro )
      ub = ubound ( samr%levels(l)%boxes(b)%hydro )

      dims = ub - lb + 1

      lbws = lbound ( ws%levels(l)%boxes(b)%U )
      ubws = ubound ( ws%levels(l)%boxes(b)%U )

      failed = &
      .not. allocated ( ws%levels(l)%boxes(b)%u ) &
      .or. size ( ws%levels(l)%boxes(b)%u ) .ne. product ( dims ) * 3 * wsid%nindices &
      .or. any ( lb .ne. lbws(:3) ) &
      .or. any ( ub .ne. ubws(:3) )
    end do
  end do
end function rhyme_mh_workspace_check_test
