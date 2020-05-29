logical function rhyme_drawing_sphere_test() result(failed)
   use rhyme_drawing_factory
   use rhyme_physics_factory
   use rhyme_initial_condition_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(shape_t), pointer :: shape
   type(physics_t) :: physics
   type(samr_t) :: samr
   type(initial_condition_t) :: ic
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8) :: prim(NCMP)
   real(kind=8) :: cons(NCMP)

   tester = .describe."drawing sphere"

   call rhyme_nombre_init

   physics = physics_factory_generate('SI')
   ic = initial_condition_factory_generate('uniform')
   logger = logger_factory_generate('default')

   call rhyme_physics_init(physics, logger)
   call rhyme_initial_condition_init(ic, samr, physics, logger)

   prim(cid%rho:cid%p) = hy_factory%generate_primitive()

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)

   call conv_prim_to_cons(prim(cid%rho:cid%p), cons(cid%rho:cid%e_tot))

   draw%type = drid%transparent_canvas

   shape => draw%new_shape(drid%sphere)
   shape%sphere%unit_str = 'm'
   shape%sphere%unit => .parse.'m'
   shape%sphere%origin = 0d0
   shape%sphere%r = 1d0
   shape%sphere%sigma = 1d-1
   shape%fill%type = drid%uniform
   shape%fill%modes(1) = drid%absolute
   shape%fill%colors(cid%rho:cid%p, 1) = prim(cid%rho:cid%p)
   shape%fill%colors(cid%p + 1:NCMP, 1) = 5.23e0
   shape%fill%colors(:, 2) = 2e0

   call rhyme_drawing_sphere(samr, ic, shape, logger)

#define IDX 1

#if NDIM == 2
#define JDX , 1
#define JDXEND , 128
#define KDX
#define KDXEND
#elif NDIM == 3
#define JDX , 1
#define JDXEND , 64
#define KDX , 1
#define KDXEND , 8
#endif

   call tester%expect(samr%levels(0)%boxes(1)%cells(IDX JDX KDX, cid%rho:cid%e_tot) .toBe.cons(cid%rho:cid%e_tot) .within.7)
   call tester%expect(samr%levels(0)%boxes(1)%cells(IDX JDX KDX, cid%e_tot + 1:NCMP) .toBe.5.23e0.within.7)
   ! TODO: add more tests

   failed = tester%failed()
end function rhyme_drawing_sphere_test
