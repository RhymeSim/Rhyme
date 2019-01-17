module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( slope_limiter_t ) :: sl
    logical :: initialized
  contains
  end type muscl_hancock_t

contains
end module rhyme_muscl_hancock
