submodule(rhyme_irs) init_submodule
contains
   module subroutine rhyme_irs_init(irs, thermo, logger)
      implicit none

      type(irs_t), intent(inout) :: irs
      type(thermo_base_t), intent(in) :: thermo
      type(logger_t), intent(inout) :: logger

      real(kind=8) :: g

      call logger%begin_section('irs')

      call logger%log('', 'n_iteration', '=', [irs%n_iteration])
      call logger%log('', 'tolerance', '=', [irs%tolerance])

      call logger%log('state_of_matter', '', '=', [thid%som_names(thermo%state_of_matter)])

      call logger%log('Setting up ideal gas coefficients...')

      g = get_gamma()
      gm1 = g - 1.d0
      gp1 = g + 1.d0
      gm1_gp1 = gm1/gp1
      gm1_2g = gm1/(2.d0*g)
      gp1_2g = gp1/(2.d0*g)
      g_inv = 1.d0/g

      call logger%end_section
   end subroutine rhyme_irs_init
end submodule init_submodule
