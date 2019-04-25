logical function rhyme_slope_limiter_test () result ( failed )
  use rhyme_slope_limiter
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sl_tester

  type ( slope_limiter_t ) :: sl

  sl_tester = .describe. "slope_limiter"

  call sl_tester%expect( slid%van_Leer .toBe. 1 )
  call sl_tester%expect( slid%minmod .toBe. 2 )
  call sl_tester%expect( slid%van_albada .toBe. 3 )
  call sl_tester%expect( slid%superbee .toBe. 4 )
  
  call sl_tester%expect( sl%w .toBe. 0.d0 )
  call sl_tester%expect( sl%type .toBe. slid%minmod )

  failed = sl_tester%failed()
end function rhyme_slope_limiter_test
