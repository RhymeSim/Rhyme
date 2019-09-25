submodule ( rhyme_nombre ) to_smod
contains
  module function rhyme_nombre_to_u ( n, u_new ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_unit_t ), target, intent ( in ) :: u_new
    type ( nombre_t ) :: n_new

    real ( kind=8 ) :: co, cn

    co = .cf. n%u
    cn = .cf. u_new

    n_new%v = n%v * co / cn

    n_new%u => u_new
  end function rhyme_nombre_to_u

  module function rhyme_nombre_to_bu ( n, u_new ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_base_unit_t ), target, intent ( in ) :: u_new
    type ( nombre_t ) :: n_new

    real ( kind=8 ) :: co, cn

    co = .cf. n%u
    cn = .cf. u_new

    n_new%v = n%v * co / cn

    n_new%u => 1 * u_new
  end function rhyme_nombre_to_bu
end submodule to_smod
