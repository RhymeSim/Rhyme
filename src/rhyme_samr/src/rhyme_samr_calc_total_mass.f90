submodule(rhyme_samr) rhyme_samr_calc_total_mass_submodule
contains
   pure module function rhyme_samr_calc_total_mass(samr) result(total_mass)
      type(samr_t), intent(in) :: samr
      real(kind=8) :: total_mass

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

      total_mass = sum( &
                   samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%rho), &
                   samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%rho) > 0d0 &
                   )

      total_mass = total_mass*product(samr%box_lengths)
   end function rhyme_samr_calc_total_mass
end submodule rhyme_samr_calc_total_mass_submodule
