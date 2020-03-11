function(calc_number_of_components n_dims n_species n_components)
  set(n_cmp 0)

  math(EXPR n_cmp "1 + ${n_dims} + 1") # rho, momenta, energy

  if(${RT_SOLVER})
    math(EXPR n_cmp "${n_cmp} + 1 + ${n_species}") # temp and nuetral fractions
  endif()

  set(${n_components}
      "${n_cmp}"
      PARENT_SCOPE)
endfunction()
