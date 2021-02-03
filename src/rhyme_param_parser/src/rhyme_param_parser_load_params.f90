submodule(rhyme_param_parser) rhyme_param_parser_load_params_submodule
contains
   module subroutine load_params( &
      param_file, chemistry, units, ic, bc, cfl, thermo, uvb, &
      ie, draw, irs, sl, mh, chombo, report, sc, logger)
      implicit none

      character(len=1024), intent(in) :: param_file
      type(chemistry_t), intent(inout) :: chemistry
      type(units_t), intent(inout) :: units
      type(initial_condition_t), intent(inout) :: ic
      type(samr_bc_t), intent(inout) :: bc
      type(cfl_t), intent(inout) :: cfl
      type(thermo_base_t), intent(inout) :: thermo
      type(uv_background_t), intent(inout) :: uvb
      type(ionisation_equilibrium_t), intent(inout) :: ie
      type(drawing_t), intent(inout) :: draw
      type(irs_t), intent(inout) :: irs
      type(slope_limiter_t), intent(inout) :: sl
      type(muscl_hancock_t), intent(inout) :: mh
      type(chombo_t), intent(inout) :: chombo
      type(report_t), intent(inout) :: report
      type(sanity_check_t), intent(inout) :: sc
      type(logger_t), intent(inout) :: logger

      type(config_t) :: config
      type(config_switch_t) :: on_off_switch, axis_switch, colormap_switch, report_switch, prim_var_switch
      type(config_switch_t) :: ic_types, ic_snapshot_types
      type(config_switch_t) :: bc_types
      type(config_switch_t) :: gas_types
      type(config_switch_t) :: canvas_types, shape_types, filling_types, filling_modes
      type(config_switch_t) :: perturb_types, perturb_method_switch
      type(config_switch_t) :: coord_types, perturb_domain_types
      type(config_switch_t) :: ionization_cases, uvb_models
      type(config_switch_t) :: limiter_types
      type(config_switch_t) :: solver_types

      type(shape_t), pointer :: shape
      type(perturbation_t), pointer :: perturb
      integer :: report_type

      integer :: shape_type, perturb_type, n_occur, i

      call logger%begin_section('params')

      call config%init(param_file)

      ! Generic switches
      call on_off_switch%add('enable', .true.)
      call on_off_switch%add('disable', .false.)
      call on_off_switch%add('true', .true.)
      call on_off_switch%add('false', .false.)
      call on_off_switch%add('.true.', .true.)
      call on_off_switch%add('.false.', .false.)
      call on_off_switch%add('on', .true.)
      call on_off_switch%add('off', .false.)

      call axis_switch%add('x', 1)
      call axis_switch%add('y', 2)
      call axis_switch%add('z', 3)

      call colormap_switch%add('rainbow', csid%rainbow)
      call colormap_switch%add('viridis', csid%viridis)
      call colormap_switch%add('magma_grey', csid%magma_grey)
      call colormap_switch%add('smooth_rainbow', csid%smooth_rainbow)

      call prim_var_switch%add('rho', cid%rho)
      call prim_var_switch%add('u', cid%u)
#if NDIM > 1
      call prim_var_switch%add('v', cid%v)
#endif
#if NDIM > 2
      call prim_var_switch%add('w', cid%w)
#endif
      call prim_var_switch%add('p', cid%p)

      ! Logging
      call config%read('unicode_plotting'.at.1, logger%unicode_plotting, logger, on_off_switch)
      call config%read('projection_axis'.at.1, logger%projection_axis, logger, axis_switch)
      call config%read('colormap'.at.1, logger%colormap, logger, colormap_switch)

      ! Report
      call report_switch%add('p-temp', repid%p_temp)
      call report_switch%add('rho-temp', repid%rho_temp)
      call report_switch%add('v2', repid%v2)
      call report_switch%add('|v|', repid%abs_v)
      call report_switch%add('w', repid%w)
      call report_switch%add('v', repid%v)
      call report_switch%add('u', repid%u)
      call report_switch%add('rho', repid%rho)
      call report_switch%add('rho_u', repid%rho_u)
#if NDIM > 1
      call report_switch%add('rho_v', repid%rho_v)
#endif
#if NDIM > 2
      call report_switch%add('rho_w', repid%rho_w)
#endif
      call report_switch%add('e_tot', repid%e_tot)
      call report_switch%add('temp', repid%temp)
      call report_switch%add('ntr_frac_0', repid%ntr_frac_0)
#if NSPE > 1
      call report_switch%add('ntr_frac_1', repid%ntr_frac_1)
#endif
#if NSPE > 2
      call report_switch%add('ntr_frac_2', repid%ntr_frac_2)
#endif

      call config%read('report_frequency'.at.1, report%every, logger)

      n_occur = config%occur('report_pseudocolor')
      do i = 1, n_occur
         call config%read('report_pseudocolor'.at.1.occur.i, report_type, logger, report_switch)
         call report%new_psudocolor(report_type)
      end do

      n_occur = config%occur('report_phase_diagram')
      do i = 1, n_occur
         call config%read('report_phase_diagram'.at.1.occur.i, report_type, logger, report_switch)
         call report%new_phase_diagram(report_type)
      end do

      n_occur = config%occur('report_histogram')
      do i = 1, n_occur
         call config%read('report_histogram'.at.1.occur.i, report_type, logger, report_switch)
         call report%new_histogram(report_type)
      end do

      ! Sanity Check
      call config%read('sanity_check'.at.1, sc%enabled, logger, on_off_switch)
      if (sc%enabled) then
         call config%read('sanity_check_frequency'.at.1, sc%every, logger)

         call config%read('sanity_check_rho'.at.1, sc%properties(scid%rho), logger, on_off_switch)
         if (sc%properties(scid%rho)) then
            call config%read('sanity_check_rho'.at.2, sc%rho_range, logger)
            call config%read('sanity_check_rho'.at.4, sc%rho_unit_str, logger)
         end if

         call config%read('sanity_check_vx'.at.1, sc%properties(scid%vx), logger, on_off_switch)
         if (sc%properties(scid%vx)) then
            call config%read('sanity_check_vx'.at.2, sc%vx_range, logger)
            call config%read('sanity_check_vx'.at.4, sc%vx_unit_str, logger)
         end if

         call config%read('sanity_check_vy'.at.1, sc%properties(scid%vy), logger, on_off_switch)
         if (sc%properties(scid%vy)) then
            call config%read('sanity_check_vy'.at.2, sc%vy_range, logger)
            call config%read('sanity_check_vy'.at.4, sc%vy_unit_str, logger)
         end if

         call config%read('sanity_check_vz'.at.1, sc%properties(scid%vz), logger, on_off_switch)
         if (sc%properties(scid%vz)) then
            call config%read('sanity_check_vz'.at.2, sc%vz_range, logger)
            call config%read('sanity_check_vz'.at.4, sc%vz_unit_str, logger)
         end if

         call config%read('sanity_check_e_tot'.at.1, sc%properties(scid%e_tot), logger, on_off_switch)
         if (sc%properties(scid%e_tot)) then
            call config%read('sanity_check_e_tot'.at.2, sc%e_tot_range, logger)
            call config%read('sanity_check_e_tot'.at.4, sc%e_tot_unit_str, logger)
         end if

         call config%read('sanity_check_temp'.at.1, sc%properties(scid%temp), logger, on_off_switch)
         if (sc%properties(scid%temp)) then
            call config%read('sanity_check_temp'.at.2, sc%temp_range, logger)
            call config%read('sanity_check_temp'.at.4, sc%temp_unit_str, logger)
         end if

         call config%read('sanity_check_ntr_frac_0'.at.1, sc%properties(scid%ntr_frac_0), logger, on_off_switch)
         if (sc%properties(scid%ntr_frac_0)) then
            call config%read('sanity_check_ntr_frac_0'.at.2, sc%ntr_frac_0_range, logger)
         end if

         call config%read('sanity_check_ntr_frac_1'.at.1, sc%properties(scid%ntr_frac_1), logger, on_off_switch)
         if (sc%properties(scid%ntr_frac_1)) then
            call config%read('sanity_check_ntr_frac_1'.at.2, sc%ntr_frac_1_range, logger)
         end if

         call config%read('sanity_check_ntr_frac_2'.at.1, sc%properties(scid%ntr_frac_2), logger, on_off_switch)
         if (sc%properties(scid%ntr_frac_2)) then
            call config%read('sanity_check_ntr_frac_2'.at.2, sc%ntr_frac_2_range, logger)
         end if

         call config%read('sanity_check_abs_v'.at.1, sc%properties(scid%abs_v), logger, on_off_switch)
         if (sc%properties(scid%abs_v)) then
            call config%read('sanity_check_abs_v'.at.2, sc%abs_v_range, logger)
            call config%read('sanity_check_abs_v'.at.4, sc%abs_v_unit_str, logger)
         end if

         call config%read('sanity_check_Mach'.at.1, sc%properties(scid%mach), logger, on_off_switch)
         if (sc%properties(scid%mach)) then
            call config%read('sanity_check_Mach'.at.2, sc%mach_range, logger)
         end if

         call config%read('sanity_check_total_mass'.at.1, sc%properties(scid%total_mass), logger, on_off_switch)
         if (sc%properties(scid%total_mass)) then
            call config%read('sanity_check_total_mass'.at.2, sc%total_mass_range, logger)
         end if

         call config%read('sanity_check_total_energy'.at.1, sc%properties(scid%total_energy), logger, on_off_switch)
         if (sc%properties(scid%total_energy)) then
            call config%read('sanity_check_total_energy'.at.2, sc%total_energy_range, logger)
         end if
      end if

      ! Initial Condition
      call ic_types%add('simple', icid%simple)
      call ic_types%add('snapshot', icid%snapshot)

      call ic_snapshot_types%add('rhyme', icid%rhyme)
      call ic_snapshot_types%add('radamesh', icid%radamesh)

      call config%read('ic_type'.at.1, ic%type, logger, ic_types)
      call config%read('ic_snapshot_type'.at.1, ic%snapshot_type, logger, ic_snapshot_types)
      call config%read('ic_snapshot_path'.at.1, ic%snapshot_path, logger)
      call config%read('ic_grid'.at.1, ic%base_grid(1:NDIM), logger)
      call config%read('ic_box_lengths'.at.1, ic%box_lengths(1)%v, logger)
#if NDIM > 1
      call config%read('ic_box_lengths'.at.2, ic%box_lengths(2)%v, logger)
#endif
#if NDIM > 2
      call config%read('ic_box_lengths'.at.3, ic%box_lengths(3)%v, logger)
#endif
      call config%read('ic_box_lengths'.at.1 + NDIM, ic%box_length_unit, logger)
      call config%read('ic_nlevels'.at.1, ic%nlevels, logger)
      call config%read('max_nboxes'.at.1, ic%max_nboxes(0:ic%nlevels - 1), logger)
      call config%read('ic_redshift'.at.1, ic%redshift, logger)

      ! Boundary Conditions
      call bc_types%add('reflective', bcid%reflective)
      call bc_types%add('outflow', bcid%outflow)
      call bc_types%add('periodic', bcid%periodic)
      call bc_types%add('inflow', bcid%inflow)

      call config%read('left_bc'.at.1, bc%types(bcid%left), logger, bc_types)
      if (bc%types(bcid%left) == bcid%inflow) then
         call config%read_array('left_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%left), logger)
         call logger%log('left_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%left))
      end if

      call config%read('right_bc'.at.1, bc%types(bcid%right), logger, bc_types)
      if (bc%types(bcid%right) == bcid%inflow) then
         call config%read_array('right_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%right), logger)
         call logger%log('right_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%right))
      end if
#if NDIM > 1
      call config%read('bottom_bc'.at.1, bc%types(bcid%bottom), logger, bc_types)
      if (bc%types(bcid%bottom) == bcid%inflow) then
         call config%read_array('bottom_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%bottom), logger)
         call logger%log('bottom_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%bottom))
      end if

      call config%read('top_bc'.at.1, bc%types(bcid%top), logger, bc_types)
      if (bc%types(bcid%top) == bcid%inflow) then
         call config%read_array('top_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%top), logger)
         call logger%log('top_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%top))
      end if
#endif
#if NDIM > 2
      call config%read('back_bc'.at.1, bc%types(bcid%back), logger, bc_types)
      if (bc%types(bcid%back) == bcid%inflow) then
         call config%read_array('back_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%back), logger)
         call logger%log('back_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%back))
      end if

      call config%read('front_bc'.at.1, bc%types(bcid%front), logger, bc_types)
      if (bc%types(bcid%front) == bcid%inflow) then
         call config%read_array('front_bc'.at.2, bc%prim_inflows(1:NCMP, bcid%front), logger)
         call logger%log('front_bc', '[primitive]', '=', bc%prim_inflows(1:NCMP, bcid%front))
      end if
#endif

      ! chemistry
      call config%read_array('elements'.at.1, chemistry%element_names, logger)
      call config%read_array('element_abundances'.at.1, chemistry%element_abundances, logger)

      ! units
      call config%read('density_unit'.at.1, units%rho_str, logger)
      call config%read('length_unit'.at.1, units%length_str, logger)
      call config%read('time_unit'.at.1, units%time_str, logger)

      ! CFL
      call config%read('courant_number'.at.1, cfl%courant_number, logger)

      ! Ideal Gas
      call gas_types%add('monatomic', thid%monatomic)
      call gas_types%add('diatomic', thid%diatomic)
      call gas_types%add('polyatomic', thid%polyatomic)

      call config%read('ideal_gas_type'.at.1, thermo%state_of_matter, logger, gas_types)

      ! UV Background
      call uvb_models%add('HM12', uvbid%HM12)

      call config%read('uvb_model'.at.1, uvb%model, logger, uvb_models)

      ! Ionization equilibrium
      call ionization_cases%add('case_a', ieid%case_a)
      call ionization_cases%add('case_b', ieid%case_b)

      call config%read('uvb_equilibrium'.at.1, ie%uvb, logger, on_off_switch)
      call config%read('uvb_self_shielding'.at.1, ie%uvb_self_shielding, logger, on_off_switch)

      call config%read('collisional_ionization_equilibrium'.at.1, ie%collisional, logger, on_off_switch)

      call config%read('photoionization_equilibrium'.at.1, ie%photo, logger, on_off_switch)

      call config%read('ie_convergence_rate'.at.1, ie%convergence_rate, logger)
      call config%read('ie_max_niterations'.at.1, ie%max_niterations, logger)

      call config%read_array('species_cases'.at.1, ie%cases, logger, ionization_cases)

      call config%read_array('equilibrium_table_size'.at.1, ie%table_sizes, logger)
      call config%read('equilibrium_table_temp_range'.at.1, ie%table_temp_range(1)%v, logger)
      call config%read('equilibrium_table_temp_range'.at.2, ie%table_temp_range(2)%v, logger)
      call config%read('equilibrium_table_temp_range'.at.3, ie%table_temp_unit_str, logger)
      call config%read('equilibrium_table_density_range'.at.1, ie%table_density_range(1)%v, logger)
      call config%read('equilibrium_table_density_range'.at.2, ie%table_density_range(2)%v, logger)
      call config%read('equilibrium_table_density_range'.at.3, ie%table_density_unit_str, logger)

      ! Drawing
      call canvas_types%add('uniform', drid%uniform_canvas)
      call canvas_types%add('transparent', drid%transparent_canvas)

      call config%read('canvas'.at.1, draw%type, logger, canvas_types)
      if (draw%type .eq. drid%uniform_canvas) then
         call config%read('canvas'.at.2.hint.'color', draw%canvas(1:NCMP), logger)
      end if

      ! Shapes
      call shape_types%add('cuboid', drid%cuboid)
      call shape_types%add('sharp-cuboid', drid%sharp_cuboid)
#if NDIM > 1
      call shape_types%add('prism', drid%prism)
      call shape_types%add('smoothed_slab_2d', drid%smoothed_slab_2d)
#endif
      call shape_types%add('sphere', drid%sphere)

      n_occur = config%occur('shape')

      call filling_types%add('uniform', drid%uniform)

      call filling_modes%add('add', drid%add)
      call filling_modes%add('absolute', drid%absolute)

      do i = 1, n_occur
         call config%read('shape'.at.1.occur.i, shape_type, logger, shape_types)
         shape => draw%new_shape(shape_type)

         select case (shape_type)
         case (drid%cuboid)
            call config%read('shape'.at.2.occur.i.hint.'left_corner', shape%cuboid%left_corner(1:NDIM), logger)
            call config%read('shape'.at.2 + NDIM.occur.i.hint.'lengths', shape%cuboid%lengths(1:NDIM), logger)
            call config%read('shape'.at.2 + 2*NDIM.occur.i.hint.'sigma', shape%cuboid%sigma, logger)

            call config%read('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
            call config%read('shape_filling'.at.2.occur.i, shape%fill%modes(1), logger, filling_modes)
            call config%read_array('shape_filling'.at.3.occur.i.hint.'color_1', shape%fill%colors(cid%rho:NCMP, 1), logger)
            call config%read_array('shape_filling'.at.3 + NCMP.occur.i.hint.'color_2', shape%fill%colors(cid%rho:NCMP, 2), logger)

#if NDIM > 1
         case (drid%prism)
            call config%read('shape'.at.2.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 1), logger)
            call config%read('shape'.at.2 + 1*NDIM.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 2), logger)
            call config%read('shape'.at.2 + 2*NDIM.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 3), logger)
#if NDIM > 2
            call config%read('shape'.at.2 + 3*NDIM.occur.i.hint.'thickness', shape%prism%thickness, logger)
#endif

            call config%read('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
            call config%read('shape_filling'.at.2.occur.i.hint.'color', shape%fill%colors(cid%rho:NCMP, 1), logger)
#endif

         case (drid%sphere)
            call config%read('shape'.at.2.occur.i.hint.'origin', shape%sphere%origin(1:NDIM), logger)
            call config%read('shape'.at.2 + NDIM.occur.i.hint.'radius', shape%sphere%r, logger)
            call config%read('shape'.at.2 + NDIM + 1.occur.i.hint.'sigma', shape%sphere%sigma, logger)
            call config%read('shape'.at.2 + NDIM + 2.occur.i.hint.'unit_str', shape%sphere%unit_str, logger)

            call config%read('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
            call config%read('shape_filling'.at.2.occur.i, shape%fill%modes(1), logger, filling_modes)
            call config%read_array('shape_filling'.at.3.occur.i.hint.'color_1', shape%fill%colors(cid%rho:NCMP, 1), logger)
            call config%read_array('shape_filling'.at.3 + NCMP.occur.i.hint.'color_2', shape%fill%colors(cid%rho:NCMP, 2), logger)

#if NDIM > 1
         case (drid%smoothed_slab_2d)
            call config%read('shape'.at.2.occur.i.hint.'axis', shape%slab_2d%axis, logger, axis_switch)
            call config%read('shape'.at.3.occur.i.hint.'positions', shape%slab_2d%pos(1:2), logger)
            call config%read('shape'.at.5.occur.i.hint.'sigmas', shape%slab_2d%sigma(1:2), logger)

            call config%read('shape_filling'.at.1.occur.i.hint.'color(1)', shape%fill%colors(cid%rho:NCMP, 1), logger)
            call config%read('shape_filling'.at.1 + NCMP.occur.i.hint.'color(2)', shape%fill%colors(cid%rho:NCMP, 2), logger)
#endif
!
         case (drid%sharp_cuboid)
            call config%read('shape'.at.2.occur.i.hint.'left_corner', shape%cuboid%left_corner(1:NDIM), logger)
            call config%read('shape'.at.2 + NDIM.occur.i.hint.'lengths', shape%cuboid%lengths(1:NDIM), logger)
            call config%read('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
            call config%read_array('shape_filling'.at.2.occur.i.hint.'color_1', shape%fill%colors(cid%rho:NCMP, 1), logger)

         end select
      end do

      ! Perturbations
      call perturb_types%add('harmonic', drid%harmonic)
      call perturb_types%add('wgn', drid%wgn)
#if NDIM > 1
      call perturb_types%add('symmetric_decaying', drid%symmetric_decaying)
#endif

      call perturb_method_switch%add('Box-Muller', drid%box_muller)

      call coord_types%add('cartesian', drid%cartesian)

      call perturb_domain_types%add('x', drid%x)
#if NDIM > 1
      call perturb_domain_types%add('y', drid%y)
#endif
#if NDIM > 2
      call perturb_domain_types%add('z', drid%z)
#endif

      n_occur = config%occur('perturb')

      do i = 1, n_occur
         call config%read('perturb'.at.1.occur.i, perturb_type, logger, perturb_types)
         perturb => draw%new_perturb(perturb_type)

         select case (perturb_type)
         case (drid%harmonic)
            call config%read('perturb'.at.2.occur.i.hint.'coordinate', perturb%coor_type, logger, coord_types)
            call config%read('perturb'.at.3.occur.i.hint.'domain', perturb%axis, logger, perturb_domain_types)
            call config%read('perturb'.at.4.occur.i.hint.'A', perturb%harmonic%A, logger)
            call config%read('perturb'.at.5.occur.i.hint.'lambda', perturb%harmonic%lambda, logger)
            call config%read('perturb'.at.6.occur.i.hint.'state', perturb%harmonic%base(cid%rho:cid%p), logger)

         case (drid%wgn)
            call config%read('perturb'.at.2.occur.i.hint.'method', perturb%wgn%method, logger, perturb_method_switch)
            call config%read('perturb'.at.3.occur.i.hint.'seed', perturb%wgn%seed, logger)
            call config%read('perturb'.at.4.occur.i.hint.'variable', perturb%wgn%variable, logger, prim_var_switch)
            call config%read_array('perturb'.at.5.occur.i.hint.'range', perturb%wgn%range, logger)
            call config%read('perturb'.at.7.occur.i.hint.'standard deviation', perturb%wgn%sd, logger)
            call config%read('perturb'.at.8.occur.i.hint.'mean', perturb%wgn%mean, logger)
            call config%read('perturb'.at.9.occur.i.hint.'cut [percent]', perturb%wgn%cut_percent, logger)

#if NDIM > 1
         case (drid%symmetric_decaying)
            call config%read('perturb'.at.2.occur.i.hint.'coordinate', perturb%coor_type, logger, coord_types)
            call config%read('perturb'.at.3.occur.i.hint.'domain', perturb%axis, logger, perturb_domain_types)
            call config%read('perturb'.at.4.occur.i.hint.'A', perturb%sym_decaying%A, logger)
            call config%read('perturb'.at.5.occur.i.hint.'position', perturb%sym_decaying%pos, logger)
            call config%read('perturb'.at.6.occur.i.hint.'sigma', perturb%sym_decaying%sigma, logger)
            call config%read('perturb'.at.7.occur.i.hint.'state', perturb%sym_decaying%base(cid%rho:cid%p), logger)
#endif
         end select
      end do

      ! Iterative Riemann Solver
      call config%read('vacuum_density'.at.1, irs%w_vacuum(cid%rho), logger)
      call config%read('vacuum_pressure'.at.1, irs%w_vacuum(cid%p), logger)
      call config%read('tolerance'.at.1, irs%tolerance, logger)
      call config%read('n_iteration'.at.1, irs%n_iteration, logger)

      ! Slope limiter
      call limiter_types%add('van_leer', slid%van_Leer)
      call limiter_types%add('minmod', slid%minmod)
      call limiter_types%add('van_albada', slid%van_albada)
      call limiter_types%add('superbee', slid%superbee)

      call config%read('slope_limiter'.at.1, sl%type, logger, limiter_types)
      call config%read('slope_limiter_omega'.at.1, sl%w, logger)

      ! MUSCL-Hancock solver
      call solver_types%add('memory_intensive', mhid%memory_intensive)
      call solver_types%add('cpu_intensive', mhid%cpu_intensive)

      call config%read('solver_type'.at.1, mh%solver_type, logger, solver_types)

      ! Chombo
      call config%read('prefix'.at.1, chombo%prefix, logger)
      call config%read('nickname'.at.1, chombo%nickname, logger)

      call logger%end_section
   end subroutine load_params
end submodule rhyme_param_parser_load_params_submodule
