logical function rhyme_param_parser_load_params_test() result(failed)
   use rhyme_param_parser
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

#if NDIM == 1
#define PARAM_FILE_NAME "parameters_1d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56 ]
#define SPHERE_ORIGIN_ARRAY [ 3d0 ]
#elif NDIM == 2
#define PARAM_FILE_NAME "parameters_2d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56, 128 ]
#define SPHERE_ORIGIN_ARRAY [ 3d0, 4d0 ]
#define PRISM_VERTEX_1 [ 56d0, 1d0 ]
#define PRISM_VERTEX_2 [ 56d0, 128d0 ]
#define PRISM_VERTEX_3 [ 72d0, 1d0 ]
#elif NDIM == 3
#define PARAM_FILE_NAME "parameters_3d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56, 128, 1 ]
#define SPHERE_ORIGIN_ARRAY [ 3d0, 4d0, 5d0 ]
#define PRISM_VERTEX_1 [ 56d0, 1d0, 1d0 ]
#define PRISM_VERTEX_2 [ 56d0, 128d0, 1d0 ]
#define PRISM_VERTEX_3 [ 72d0, 1d0, 1d0 ]
#endif

   type(chemistry_t) :: chemistry
   type(units_t) :: units
   type(initial_condition_t) :: ic
   type(samr_bc_t) :: bc
   type(cfl_t) :: cfl
   type(thermo_base_t) :: thermo
   type(uv_background_t) :: uvb
   type(ionisation_equilibrium_t) :: ie
   type(drawing_t) :: draw
   type(irs_t) :: irs
   type(slope_limiter_t) :: sl
   type(muscl_hancock_t) :: mh
   type(chombo_t) :: chombo
   type(report_t) :: report
   type(logger_t) :: logger

   character(len=1024), parameter :: param_file = PARAM_FILE_NAME
   type(shape_t), pointer :: shape
   type(perturbation_t), pointer :: perturb

   tester = .describe."rhyme_param_parser_load_params"

   logger = logger_factory_generate('default')

   call load_params( &
      param_file, chemistry, units, ic, bc, cfl, thermo, &
      uvb, ie, draw, irs, sl, mh, chombo, report, logger)

   ! Logging
   call tester%expect(logger%unicode_plotting.toBe..true..hint.'Logging unicode plotting')
   call tester%expect(logger%projection_axis.toBe.lgid%z.hint.'Logging projection axis')
   call tester%expect(logger%colormap.toBe.csid%viridis.hint.'Logging projection axis')

   ! Report
   call tester%expect(report%every.toBe.100.hint.'Report frequency')
   call tester%expect(report%pseudocolors%type.toBe.repid%rho.hint.'Report pseudocolors rho')
   call tester%expect(report%pseudocolors%next%type.toBe.repid%temp.hint.'Report pseudocolors temp')
   call tester%expect(report%pseudocolors%next%next%type.toBe.repid%ntr_frac_0.hint.'Report pseudocolors ntr_frac_0')
   call tester%expect(associated(report%pseudocolors%next%next%next) .toBe..false..hint.'Report pseudocolors last')
   call tester%expect(report%phase_diagrams%type.toBe.repid%rho_temp.hint.'Report phase diagrams rho-temp')
   call tester%expect(report%phase_diagrams%next%type.toBe.repid%p_temp.hint.'Report phase diagrams p-temp')
   call tester%expect(associated(report%phase_diagrams%next%next) .toBe..false..hint.'Report phase diagrams last')
   call tester%expect(report%histograms%type.toBe.repid%abs_v.hint.'Report histograms |v|')
   call tester%expect(report%histograms%next%type.toBe.repid%e_tot.hint.'Report histograms e_tot')
   call tester%expect(associated(report%histograms%next%next) .toBe..false..hint.'Report histograms last')

   ! Structured AMR
   call tester%expect(ic%type.toBe.icid%simple.hint.'IC type')
   call tester%expect(ic%base_grid.toBe.128.hint.'IC grid')
   call tester%expect((ic%box_lengths%v) .toBe.1d0.hint.'IC length')
   call tester%expect(ic%box_length_unit.toBe.'kpc'.hint.'IC length unit')
   call tester%expect(ic%nlevels.toBe.3.hint.'IC nlevels')
   call tester%expect(ic%max_nboxes(0:ic%nlevels - 1) .toBe. [1, 10, 100] .hint.'IC max_nboxes')
   call tester%expect(ic%max_nboxes(ic%nlevels:) .toBe.0.hint.'IC max_nboxes unused')
   call tester%expect(ic%redshift.toBe.1.23d0.hint.'IC redshift')

   ! Boundary Condition
   call tester%expect(bc%types(bcid%left) .toBe.1)
   call tester%expect(bc%types(bcid%right) .toBe.2)
#if NDIM > 1
   call tester%expect(bc%types(bcid%bottom) .toBe.3)
   call tester%expect(bc%types(bcid%top) .toBe.1)
#endif
#if NDIM > 2
   call tester%expect(bc%types(bcid%back) .toBe.2)
   call tester%expect(bc%types(bcid%front) .toBe.3)
#endif

   ! units
   call tester%expect(units%rho_str.toBe.'kg / m^3')
   call tester%expect(units%length_str.toBe.'m')
   call tester%expect(units%time_str.toBe.'s')

   ! CFL
   call tester%expect(cfl%courant_number.toBe..2d0)

   ! Ideal Gas
   call tester%expect(thermo%state_of_matter.toBe.thid%diatomic)

   ! Chemistry
   call tester%expect(chemistry%element_names.toBe. ['H ', 'He'])
   call tester%expect(chemistry%element_abundances.toBe. [.75e0, .25e0])

   ! UV background
   call tester%expect(uvb%model.toBe.uvbid%HM12.hint.'UVB Model')

   ! Ionization equilibrium
   call tester%expect(ie%uvb.toBe..true..hint.'IE UVB')
   call tester%expect(ie%uvb_self_shielding.toBe..true..hint.'IE UVB Self-Shielding')

   call tester%expect(ie%collisional.toBe..true..hint.'IE Collisional Ionization')

   call tester%expect(ie%photo.toBe..false..hint.'IE Photo-Ionization')

   call tester%expect(ie%convergence_rate.toBe.1e-2.hint.'IE convergence rate')
   call tester%expect(ie%max_niterations.toBe.1000.hint.'IE maximum niterations')

   call tester%expect(ie%cases.toBe. [ieid%case_a, ieid%case_b, ieid%case_a] .hint.'IE cases')

   call tester%expect(ie%table_sizes.toBe. [1024, 1024] .hint.'IE table size')
   call tester%expect(ie%table_temp_range(:)%v.toBe. [1e2, 1e7] .hint.'IE table temp range')
   call tester%expect(ie%table_temp_unit_str.toBe.'K'.hint.'IE table temp unit')
   call tester%expect(ie%table_density_range(:)%v.toBe. [1d-3, 1d6] .hint.'IE table density range')
   call tester%expect(ie%table_density_unit_str.toBe.'m_H / cm^3'.hint.'IE table density unit')

   ! Drawing
   call tester%expect(draw%type.toBe.drid%uniform_canvas)
   call tester%expect(draw%canvas(cid%rho) .toBe..125d0)
   call tester%expect(draw%canvas(cid%u:cid%u + NDIM - 1) .toBe.0d0)
   call tester%expect(draw%canvas(cid%p) .toBe..1d0)
   call tester%expect(draw%canvas(cid%temp) .toBe.1d4)
   call tester%expect(draw%canvas(cid%ntr_frac_0) .toBe.1d-2)
#if NSPE > 1
   call tester%expect(draw%canvas(cid%ntr_frac_1) .toBe.2d-3)
#endif
#if NSPE > 2
   call tester%expect(draw%canvas(cid%ntr_frac_2) .toBe.3d-4)
#endif

   shape => draw%shapes
   call tester%expect(shape%type.toBe.drid%cuboid)
   call tester%expect(shape%cuboid%left_corner.toBe.1)
   call tester%expect(shape%cuboid%lengths.toBe.CUBOID_LENGTH_ARRAY.hint.'cuboid length array')
   call tester%expect(shape%cuboid%sigma.toBe.2.34d-1)
   call tester%expect(shape%fill%type.toBe.drid%uniform)
   call tester%expect(shape%fill%colors(cid%rho:cid%rho, 1) .toBe.1d0)
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 1) .toBe.0d0)
   call tester%expect(shape%fill%colors(cid%p, 1) .toBe.1d0)
   call tester%expect(shape%fill%colors(cid%temp, 1) .toBe.1d4)
   call tester%expect(shape%fill%colors(cid%ntr_frac_0, 1) .toBe.1d-2)
#if NSPE > 1
   call tester%expect(shape%fill%colors(cid%ntr_frac_1, 1) .toBe.1d-3)
#endif
#if NSPE > 2
   call tester%expect(shape%fill%colors(cid%ntr_frac_2, 1) .toBe.1d-4)
#endif
   call tester%expect(shape%fill%colors(cid%rho, 2) .toBe.2d0.hint.'color_2 rho')
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 2) .toBe.3d0)
   call tester%expect(shape%fill%colors(cid%p, 2) .toBe.4d0)
#if NSPE > 1
   call tester%expect(shape%fill%colors(cid%ntr_frac_1, 2) .toBe.1d-4.hint.'cuboid color_2 ntr_frac_1')
#endif
#if NSPE > 2
   call tester%expect(shape%fill%colors(cid%ntr_frac_2, 2) .toBe.1d-5.hint.'cuboid color_2 ntr_frac_2')
#endif

   ! sharp cuboid
   shape => shape%next
   call tester%expect(shape%type.toBe.drid%sharp_cuboid.hint.'sharp_cuboid')

   ! sphere
   shape => shape%next
   call tester%expect(shape%type.toBe.drid%sphere.hint.'sphere')
   call tester%expect(shape%sphere%origin.toBe.SPHERE_ORIGIN_ARRAY)
   call tester%expect(shape%sphere%r.toBe.2.34d0)
   call tester%expect(shape%sphere%sigma.toBe.2.34d-1)
   call tester%expect(shape%sphere%unit_str.toBe.'unit')
   call tester%expect(shape%fill%type.toBe.drid%uniform)
   call tester%expect(shape%fill%modes(1) .toBe.drid%absolute)
   call tester%expect(shape%fill%colors(cid%rho, 1) .toBe.1d0)
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 1) .toBe.0d0)
   call tester%expect(shape%fill%colors(cid%p, 1) .toBe.1d0)
   call tester%expect(shape%fill%colors(cid%temp, 1) .toBe.1d4)
   call tester%expect(shape%fill%colors(cid%rho, 2) .toBe.2d0)
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 2) .toBe.3d0)
   call tester%expect(shape%fill%colors(cid%p, 2) .toBe.4d0)

#if NDIM > 1
   ! prism
   shape => shape%next
   call tester%expect(shape%type.toBe.drid%prism.hint.'prism')
   call tester%expect(shape%prism%vertices(:, 1) .toBe.PRISM_VERTEX_1)
   call tester%expect(shape%prism%vertices(:, 2) .toBe.PRISM_VERTEX_2)
   call tester%expect(shape%prism%vertices(:, 3) .toBe.PRISM_VERTEX_3)
#if NDIM > 2
   call tester%expect(shape%prism%thickness.toBe.1.0.hint.'thickness')
#endif

   ! smoothed_slab_2d
   shape => shape%next
   call tester%expect(shape%type.toBe.drid%smoothed_slab_2d.hint.'smoothed_slab')
   call tester%expect(shape%slab_2d%axis.toBe.drid%x)
   call tester%expect(shape%slab_2d%pos.toBe. [56.0, 72.0])
   call tester%expect(shape%slab_2d%sigma.toBe. [.2d0, .4d0])
   call tester%expect(shape%fill%colors(cid%rho, 1) .toBe..125d0)
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 1) .toBe.0d0)
   call tester%expect(shape%fill%colors(cid%p, 1) .toBe..1d0)
   call tester%expect(shape%fill%colors(cid%rho, 2) .toBe.1d0)
   call tester%expect(shape%fill%colors(cid%u:cid%u + NDIM - 1, 2) .toBe.0d0)
   call tester%expect(shape%fill%colors(cid%p, 2) .toBe.1d0)
#endif

   ! Perturbation
   perturb => draw%perturbs
   call tester%expect(perturb%type.toBe.drid%harmonic.hint.'harmonic')
   call tester%expect(perturb%coor_type.toBe.drid%cartesian)
   call tester%expect(perturb%axis.toBe.drid%x)
   call tester%expect(perturb%harmonic%A.toBe..05d0)
   call tester%expect(perturb%harmonic%lambda.toBe.32.0)
   call tester%expect(perturb%harmonic%base(cid%rho) .toBe.0d0)
   call tester%expect(perturb%harmonic%base(cid%u:cid%u + NDIM - 1) .toBe.0d0)
   call tester%expect(perturb%harmonic%base(cid%p) .toBe.1d0)

#if NDIM > 1
  perturb => perturb%next
   call tester%expect(perturb%type.toBe.drid%symmetric_decaying)
   call tester%expect(perturb%coor_type.toBe.drid%cartesian)
   call tester%expect(perturb%axis.toBe.drid%y)
   call tester%expect(perturb%sym_decaying%A.toBe.1d0)
   call tester%expect(perturb%sym_decaying%pos.toBe.56.0)
   call tester%expect(perturb%sym_decaying%sigma.toBe.2.0)
   call tester%expect(perturb%sym_decaying%base(cid%rho) .toBe.0d0)
   call tester%expect(perturb%sym_decaying%base(cid%u:cid%u + NDIM - 1) .toBe.0d0)
   call tester%expect(perturb%sym_decaying%base(cid%p) .toBe.1d0)

   perturb => perturb%next
   call tester%expect(perturb%type.toBe.drid%symmetric_decaying)
   call tester%expect(perturb%coor_type.toBe.drid%cartesian)
   call tester%expect(perturb%axis.toBe.drid%y)
   call tester%expect(perturb%sym_decaying%A.toBe.1d0)
   call tester%expect(perturb%sym_decaying%pos.toBe.72.0)
   call tester%expect(perturb%sym_decaying%sigma.toBe.8.0)
   call tester%expect(perturb%sym_decaying%base(cid%rho) .toBe.0d0)
   call tester%expect(perturb%sym_decaying%base(cid%u:cid%u + NDIM - 1) .toBe.0d0)
   call tester%expect(perturb%sym_decaying%base(cid%p) .toBe.1d0)
#endif

   perturb => perturb%next
   call tester%expect(perturb%type.toBe.drid%wgn.hint.'wgn type')
   call tester%expect(perturb%wgn%method.toBe.drid%box_muller.hint.'wgn method')
   call tester%expect(perturb%wgn%seed.toBe.2345.hint.'wgn seed')
   call tester%expect(perturb%wgn%variable.toBe.cid%rho.hint.'wgn variable')
   call tester%expect(perturb%wgn%range.toBe.[0d0, 1d2].hint.'wgn range')
   call tester%expect(perturb%wgn%sd.toBe.1d0.hint.'wgn standard deviation')
   call tester%expect(perturb%wgn%mean.toBe.0d0.hint.'wgn mean')
   call tester%expect(perturb%wgn%cut_percent.toBe.30.hint.'wgn cut (percent)')

   ! Iterative Riemann Solver
   call tester%expect(irs%w_vacuum(cid%p) .toBe.1.23d0)
   call tester%expect(irs%w_vacuum(cid%rho) .toBe.2.34d0)
   call tester%expect(irs%tolerance.toBe.1d-6)
   call tester%expect(irs%n_iteration.toBe.100)

   ! Slope Limiter
   call tester%expect(sl%type.toBe.slid%minmod.hint.'Slope limiter type')
   call tester%expect(sl%w.toBe.-1d0.hint.'Slope limiter omega')

   ! MUSCL-Hancock solver
   call tester%expect(mh%solver_type.toBe.mhid%cpu_intensive)

   ! Chombo
   call tester%expect(trim(chombo%prefix) .toBe."./prefix")
   call tester%expect(trim(chombo%nickname) .toBe."hydro-simulation")

   failed = tester%failed()
end function rhyme_param_parser_load_params_test
