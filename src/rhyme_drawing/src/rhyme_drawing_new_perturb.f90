submodule ( rhyme_drawing ) new_perturb_smod
contains
  module function rhyme_drawing_new_perturb ( this, perturb_type ) result ( perturb )
    implicit none

    class ( drawing_t ), intent ( inout ) :: this
    integer, intent ( in ) :: perturb_type

    type ( perturbation_t ), pointer :: perturb

    perturb => this%perturbs

    if ( associated ( perturb ) ) then
      do while ( associated ( perturb%next ) )
        perturb => perturb%next
      end do

      allocate( perturb%next )
      perturb => perturb%next
    else
      allocate( this%perturbs )
      perturb => this%perturbs
    end if

    perturb%type = perturb_type
    perturb%coor_type = drid%unset
    perturb%axis = drid%unset

    perturb%harmonic%A = 1.d0
    perturb%harmonic%lambda = 0.d0
    perturb%harmonic%base = 0.d0

#if NDIM > 1
    perturb%sym_decaying%A = 1.d0
    perturb%sym_decaying%pos = 0.d0
    perturb%sym_decaying%sigma = 1.d0
#endif
  end function rhyme_drawing_new_perturb
end submodule new_perturb_smod
