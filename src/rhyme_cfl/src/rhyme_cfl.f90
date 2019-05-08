module rhyme_cfl
  use rhyme_samr
  use rhyme_ideal_gas

  implicit none

  type cfl_t
    real ( kind=8 ) :: courant_number = .8d0
  end type cfl_t


  interface
    pure module function rhyme_cfl_time_step ( cfl, ig, samr ) result ( dt )
      class ( cfl_t ), intent(in) :: cfl
      type ( ideal_gas_t ), intent(in) :: ig
      type ( samr_t ), intent(in) :: samr
      real ( kind=8 ) :: dt
    end function rhyme_cfl_time_step
  end interface

end module rhyme_cfl
