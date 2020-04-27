submodule(rhyme_param_parser) rhyme_param_parser_load_params_submodule
contains
module subroutine load_params(param_file, physics, ic, bc, cfl, &
                              thermo, draw, irs, sl, mh, chombo, logger)
   implicit none

   character(len=1024), intent(in) :: param_file
   type(physics_t), intent(inout) :: physics
   type(initial_condition_t), intent(inout) :: ic
   type(samr_bc_t), intent(inout) :: bc
   type(cfl_t), intent(inout) :: cfl
   type(thermo_base_t), intent(inout) :: thermo
   type(drawing_t), intent(inout) :: draw
   type(irs_t), intent(inout) :: irs
   type(slope_limiter_t), intent(inout) :: sl
   type(muscl_hancock_t), intent(inout) :: mh
   type(chombo_t), intent(inout) :: chombo
   type(logger_t), intent(inout) :: logger

   type(config_t) :: config
   type(config_switch_t) :: ic_types, ic_snapshot_types
   type(config_switch_t) :: bc_types
   type(config_switch_t) :: gas_types
   type(config_switch_t) :: canvas_types, shape_types, filling_types, filling_modes
   type(config_switch_t) :: perturb_types, coord_types, perturb_domain_types
   type(config_switch_t) :: limiter_types
   type(config_switch_t) :: solver_types
   type(config_switch_t) :: axes

   type(shape_t), pointer :: shape
   type(perturbation_t), pointer :: perturb

   integer :: shape_type, perturb_type, n_occur, i

   call logger%begin_section('params')

   call config%init(param_file)

   ! Initial Condition
   call ic_types%add('simple', icid%simple)
   call ic_types%add('snapshot', icid%snapshot)

   call ic_snapshot_types%add('rhyme', icid%rhyme)
   call ic_snapshot_types%add('radamesh', icid%radamesh)

   call config%read ('ic_type'.at.1, ic%type, logger, ic_types)
   call config%read ('ic_snapshot_type'.at.1, ic%snapshot_type, logger, ic_snapshot_types)
   call config%read ('ic_snapshot_path'.at.1, ic%snapshot_path, logger)
   call config%read ('ic_grid'.at.1, ic%base_grid(1:NDIM), logger)
   call config%read ('ic_box_lengths'.at.1, ic%box_lengths(1)%v, logger)
#if NDIM > 1
   call config%read ('ic_box_lengths'.at.2, ic%box_lengths(2)%v, logger)
#endif
#if NDIM > 2
   call config%read ('ic_box_lengths'.at.3, ic%box_lengths(3)%v, logger)
#endif
   call config%read ('ic_box_lengths'.at.1 + NDIM, ic%box_length_unit, logger)
   call config%read ('ic_nlevels'.at.1, ic%nlevels, logger)
   call config%read ('max_nboxes'.at.1, ic%max_nboxes(0:ic%nlevels - 1), logger)

   ! Boundary Conditions
   call bc_types%add('reflective', bcid%reflective)
   call bc_types%add('outflow', bcid%outflow)
   call bc_types%add('periodic', bcid%periodic)

   call config%read ('left_bc'.at.1, bc%types(bcid%left), logger, bc_types)
   call config%read ('right_bc'.at.1, bc%types(bcid%right), logger, bc_types)
#if NDIM > 1
   call config%read ('bottom_bc'.at.1, bc%types(bcid%bottom), logger, bc_types)
   call config%read ('top_bc'.at.1, bc%types(bcid%top), logger, bc_types)
#endif
#if NDIM > 2
   call config%read ('back_bc'.at.1, bc%types(bcid%back), logger, bc_types)
   call config%read ('front_bc'.at.1, bc%types(bcid%front), logger, bc_types)
#endif

   ! Physics
   call config%read ('density_unit'.at.1, physics%rho_str, logger)
   call config%read ('length_unit'.at.1, physics%length_str, logger)
   call config%read ('time_unit'.at.1, physics%time_str, logger)

   ! CFL
   call config%read ('courant_number'.at.1, cfl%courant_number, logger)

   ! Ideal Gas
   call gas_types%add('monatomic', thid%monatomic)
   call gas_types%add('diatomic', thid%diatomic)
   call gas_types%add('polyatomic', thid%polyatomic)

   call config%read ('ideal_gas_type'.at.1, thermo%state_of_matter, logger, gas_types)

   ! Drawing
   call canvas_types%add('uniform', drid%uniform_canvas)
   call canvas_types%add('transparent', drid%transparent_canvas)

   call config%read ('canvas'.at.1, draw%type, logger, canvas_types)
   if (draw%type .eq. drid%uniform_canvas) then
      call config%read ('canvas'.at.2.hint.'color', draw%canvas(1:NCMP), logger)
   end if

   ! Shapes
   call shape_types%add('cuboid', drid%cuboid)
#if NDIM > 1
   call shape_types%add('prism', drid%prism)
   call shape_types%add('smoothed_slab_2d', drid%smoothed_slab_2d)
#endif
   call shape_types%add('sphere', drid%sphere)

   n_occur = config%occur('shape')

   call axes%add('x', samrid%x)
#if NDIM > 1
   call axes%add('y', samrid%y)
#endif
#if NDIM > 2
   call axes%add('z', samrid%z)
#endif

   call filling_types%add('uniform', drid%uniform)

   call filling_modes%add('add', drid%add)
   call filling_modes%add('absolute', drid%absolute)

   do i = 1, n_occur
      call config%read ('shape'.at.1.occur.i, shape_type, logger, shape_types)
      shape => draw%new_shape(shape_type)

      select case (shape_type)
      case (drid%cuboid)
         call config%read ('shape'.at.2.occur.i.hint.'left_corner', shape%cuboid%left_corner(1:NDIM), logger)
         call config%read ('shape'.at.2 + NDIM.occur.i.hint.'lengths', shape%cuboid%lengths(1:NDIM), logger)
         call config%read ('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
         call config%read ('shape_filling'.at.2.occur.i.hint.'color', shape%fill%colors(cid%rho:NCMP, 1), logger)

#if NDIM > 1
      case (drid%prism)
         call config%read ('shape'.at.2.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 1), logger)
         call config%read ('shape'.at.2 + 1*NDIM.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 2), logger)
         call config%read ('shape'.at.2 + 2*NDIM.occur.i.hint.'vertex', shape%prism%vertices(1:NDIM, 3), logger)
#if NDIM > 2
         call config%read ('shape'.at.2 + 3*NDIM.occur.i.hint.'thickness', shape%prism%thickness, logger)
#endif
#endif
         call config%read ('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
         call config%read ('shape_filling'.at.2.occur.i.hint.'color', shape%fill%colors(cid%rho:NCMP, 1), logger)

      case (drid%sphere)
         call config%read ('shape'.at.2.occur.i.hint.'origin', shape%sphere%origin(1:NDIM), logger)
         call config%read ('shape'.at.2 + NDIM.occur.i.hint.'radius', shape%sphere%r, logger)
         call config%read ('shape'.at.2 + NDIM + 1.occur.i.hint.'sigma', shape%sphere%sigma, logger)
         call config%read ('shape'.at.2 + NDIM + 2.occur.i.hint.'unit_str', shape%sphere%unit_str, logger)

         call config%read ('shape_filling'.at.1.occur.i, shape%fill%type, logger, filling_types)
         call config%read ('shape_filling'.at.2.occur.i, shape%fill%modes(1), logger, filling_modes)
         call config%read ('shape_filling'.at.3.occur.i.hint.'color_1', shape%fill%colors(cid%rho:NCMP, 1), logger)
         call config%read ('shape_filling'.at.3 + NCMP.occur.i.hint.'color_2', shape%fill%colors(cid%rho:NCMP, 2), logger)

#if NDIM > 1
      case (drid%smoothed_slab_2d)
         call config%read ('shape'.at.2.occur.i.hint.'axis', shape%slab_2d%axis, logger, axes)
         call config%read ('shape'.at.3.occur.i.hint.'positions', shape%slab_2d%pos(1:2), logger)
         call config%read ('shape'.at.5.occur.i.hint.'sigmas', shape%slab_2d%sigma(1:2), logger)

         call config%read ('shape_filling'.at.1.occur.i.hint.'color(1)', shape%fill%colors(cid%rho:NCMP, 1), logger)
         call config%read ('shape_filling'.at.1 + NCMP.occur.i.hint.'color(2)', shape%fill%colors(cid%rho:NCMP, 2), logger)
#endif

      end select
   end do

   ! Perturbations
   call perturb_types%add('harmonic', drid%harmonic)
#if NDIM > 1
   call perturb_types%add('symmetric_decaying', drid%symmetric_decaying)
#endif

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
      call config%read ('perturb'.at.1.occur.i, perturb_type, logger, perturb_types)
      perturb => draw%new_perturb(perturb_type)

      call config%read ('perturb'.at.2.occur.i.hint.'coordinate', perturb%coor_type, logger, coord_types)
      call config%read ('perturb'.at.3.occur.i.hint.'domain', perturb%axis, logger, perturb_domain_types)

      select case (perturb_type)
      case (drid%harmonic)
         call config%read ('perturb'.at.4.occur.i.hint.'A', perturb%harmonic%A, logger)
         call config%read ('perturb'.at.5.occur.i.hint.'lambda', perturb%harmonic%lambda, logger)
         call config%read ('perturb'.at.6.occur.i.hint.'state', perturb%harmonic%base(cid%rho:cid%p), logger)

#if NDIM > 1
      case (drid%symmetric_decaying)
         call config%read ('perturb'.at.4.occur.i.hint.'A', perturb%sym_decaying%A, logger)
         call config%read ('perturb'.at.5.occur.i.hint.'position', perturb%sym_decaying%pos, logger)
         call config%read ('perturb'.at.6.occur.i.hint.'sigma', perturb%sym_decaying%sigma, logger)
         call config%read ('perturb'.at.7.occur.i.hint.'state', perturb%sym_decaying%base(cid%rho:cid%p), logger)
#endif
      end select
   end do

   ! Iterative Riemann Solver
   call config%read ('vacuum_density'.at.1, irs%w_vacuum(cid%rho), logger)
   call config%read ('vacuum_pressure'.at.1, irs%w_vacuum(cid%p), logger)
   call config%read ('tolerance'.at.1, irs%tolerance, logger)
   call config%read ('n_iteration'.at.1, irs%n_iteration, logger)

   ! Slope limiter
   call limiter_types%add('van_leer', slid%van_Leer)
   call limiter_types%add('minmod', slid%minmod)
   call limiter_types%add('van_albada', slid%van_albada)
   call limiter_types%add('superbee', slid%superbee)

   call config%read ('slope_limiter'.at.1, sl%type, logger, limiter_types)
   call config%read ('slope_limiter_omega'.at.1, sl%w, logger)

   ! MUSCL-Hancock solver
   call solver_types%add('memory_intensive', mhid%memory_intensive)
   call solver_types%add('cpu_intensive', mhid%cpu_intensive)

   call config%read ('solver_type'.at.1, mh%solver_type, logger, solver_types)

   ! Chombo
   call config%read ('prefix'.at.1, chombo%prefix, logger)
   call config%read ('nickname'.at.1, chombo%nickname, logger)

   call logger%end_section
end subroutine load_params
end submodule rhyme_param_parser_load_params_submodule
