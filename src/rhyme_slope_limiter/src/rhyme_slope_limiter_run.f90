submodule ( rhyme_slope_limiter ) rhyme_sl_run_smod
contains
  pure module subroutine rhyme_slope_limiter_run ( sl, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent ( in ) :: sl
    type ( hydro_conserved_t ), intent ( in ) :: UL, U, UR
    type ( hydro_conserved_t ), intent ( out ) :: delta

    select case ( sl%type )
    case ( slid%van_Leer )
      call rhyme_slope_limiter_van_leer( sl, UL, U, UR, delta )
    case ( slid%minmod )
      call rhyme_slope_limiter_minmod( sl, UL, U, UR, delta )
    case ( slid%van_albada )
      call rhyme_slope_limiter_van_albada( sl, UL, U, UR, delta )
    case ( slid%superbee )
      call rhyme_slope_limiter_superbee( sl, UL, U, UR, delta )
    end select
  end subroutine rhyme_slope_limiter_run
end submodule rhyme_sl_run_smod
