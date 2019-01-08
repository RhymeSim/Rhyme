logical function rhyme_slope_limiter_test () result ( failed )
  use rhyme_slope_limiter

  implicit none

  type ( slope_limiter_t ) :: sl

  failed = &
  slid%van_Leer .ne. 1 &
  .or. slid%minmod .ne. 2 &
  .or. slid%van_albada .ne. 3 &
  .or. slid%superbee .ne. 4

  if ( failed ) return

  failed = &
  abs ( sl%w ) > epsilon(0.d0) &
  .or. sl%type .ne. slid%minmod
end function rhyme_slope_limiter_test
