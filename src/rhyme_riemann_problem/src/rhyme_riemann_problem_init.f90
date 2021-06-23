submodule(rhyme_riemann_problem) init_submodule
contains
   module subroutine rhyme_riemann_problem_init(rp, units, thermo, logger)
      implicit none

      type(riemann_problem_t), intent(inout) :: rp
      type(units_t), intent(in) :: units
      type(thermo_base_t), intent(in) :: thermo
      type(logger_t), intent(inout) :: logger

      real(kind=8) :: g

      call logger%begin_section('rp')

      call logger%log('', 'vacuum_density', '=', [rp%w_vacuum(cid%rho)])
      call logger%log('', 'vacuum_pressure', '=', [rp%w_vacuum(cid%p)])

      call logger%log('Setting up ideal gas coefficients...')

      g = get_gamma()

      gm1 = g - 1.d0
      gp1 = g + 1.d0
      gm1_gp1 = gm1/gp1
      gm1_2g = gm1/(2.d0*g)
      gp1_2g = gp1/(2.d0*g)
      g_inv = 1.d0/g

      select case (rp%solver)
      case (rpid%exact_rs)
         call logger%log('Initializing', '', ':', ['Exact Riemann Solver'])
         call rhyme_irs_init(rp%irs, thermo, logger)
         ! case (rpid%deep_rs)
         ! call logger%log('Initializing', '', ':', ['DeepRS'])
         ! call rhyme_deep_rs_init(rp%drs, units, logger)
      case default
         call logger%err('Unknown solver', 'suported solvers', ':', ['ExactRS'])
      end select

      call logger%end_section
   end subroutine rhyme_riemann_problem_init
end submodule init_submodule
