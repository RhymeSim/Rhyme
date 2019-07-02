function( calc_number_of_components n_dims n_species n_components )
  if( ${HYDRO_SOLVER} )
    # Adding momenta and total energy
    math( EXPR n_cmp "1 + ${n_dims} + 1" ) # rho, momenta, energy
  endif()

  if( ${RT_SOLVER} )
    # Adding temperature and ionization fractions
    math( EXPR n_cmp "${n_cmp} + 1 + ${n_species}" ) # temperature, neutral fractions
  endif()

  set( ${n_components} "${n_cmp}" PARENT_SCOPE)
endfunction()
