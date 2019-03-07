logical function rhyme_ideal_gas_flux_at_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( hydro_flux_t ) :: f

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%flux_at( hy%cons, hyid%y, f )

  failed = &
       abs ( f%f( hyid%rho ) - hy%rho * hy%v ) > epsilon(0.d0) &
  .or. abs ( f%f( hyid%rho_u ) - hy%rho * hy%u * hy%v ) > epsilon(0.d0) &
  .or. abs ( f%f( hyid%rho_v ) - (hy%rho * hy%v**2 + hy%p) ) > epsilon(0.d0) &
  .or. abs ( f%f( hyid%rho_w ) - hy%rho * hy%w * hy%v ) > epsilon(0.d0) &
  .or. abs ( f%f( hyid%e_tot ) - hy%v * ( hy%e_tot + hy%p ) ) > epsilon(0.d0)
end function rhyme_ideal_gas_flux_at_test
