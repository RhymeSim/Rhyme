logical function rhyme_drawing_new_perturb_test () result ( failed )
  use rhyme_drawing

  implicit none

  type ( drawing_t ) :: draw
  type ( perturbation_t ), pointer :: perturb

  perturb => draw%new_perturb( drid%harmonic )

  failed = &
  .not. associated( draw%perturbs ) &
  .or. associated( draw%perturbs%next ) &
  .or. .not. associated( perturb ) &
  .or. draw%perturbs%type .ne. drid%harmonic &
  .or. perturb%type .ne. drid%harmonic &
  .or. draw%perturbs%coor_type .ne. drid%unset &
  .or. perturb%coor_type .ne. drid%unset &
  .or. draw%perturbs%dir .ne. drid%unset &
  .or. perturb%dir .ne. drid%unset &
  .or. abs( draw%perturbs%harmonic%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( perturb%harmonic%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%harmonic%lambda ) > epsilon(0.d0) &
  .or. abs( perturb%harmonic%lambda ) > epsilon(0.d0) &
  .or. any( abs( draw%perturbs%harmonic%base%w ) > epsilon(0.d0) ) &
  .or. any( abs( perturb%harmonic%base%w ) > epsilon(0.d0) ) &
  .or. abs( draw%perturbs%sym_decaying%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( perturb%sym_decaying%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%sym_decaying%pos ) > epsilon(0.d0) &
  .or. abs( perturb%sym_decaying%pos ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%sym_decaying%sigma - 1.d0 ) > epsilon(0.d0) &
  .or. abs( perturb%sym_decaying%sigma - 1.d0 ) > epsilon(0.d0) &
  .or. any( abs( draw%perturbs%sym_decaying%base%w ) > epsilon(0.d0) ) &
  .or. any( abs( perturb%sym_decaying%base%w ) > epsilon(0.d0) )
  if ( failed ) return


  perturb => draw%new_perturb( drid%symmetric_decaying )

  failed = &
  .not. associated( draw%perturbs%next ) &
  .or. .not. associated( perturb ) &
  .or. draw%perturbs%next%type .ne. drid%symmetric_decaying &
  .or. perturb%type .ne. drid%symmetric_decaying
end function rhyme_drawing_new_perturb_test
