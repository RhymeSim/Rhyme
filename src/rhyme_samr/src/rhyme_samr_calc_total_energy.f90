submodule(rhyme_samr) rhyme_samr_calc_total_energy_submodule
contains
   pure module function rhyme_samr_calc_total_energy(samr) result(total_energy)
      type(samr_t), intent(in) :: samr
      real(kind=8) :: total_energy

#if NDIM == 1
#define JDX
#define KDX
#endif
#if NDIM == 2
#define JDX , :
#define KDX
#endif
#if NDIM == 3
#define JDX , :
#define KDX , :
#endif

      total_energy = sum( &
                     samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%e_tot), &
                     samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%e_tot) > 0d0 &
                     )

      total_energy = total_energy*product(samr%box_lengths)
   end function rhyme_samr_calc_total_energy
end submodule rhyme_samr_calc_total_energy_submodule
