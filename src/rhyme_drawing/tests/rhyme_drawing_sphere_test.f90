logical function rhyme_drawing_sphere_test() result(failed)
   use rhyme_drawing
   use rhyme_physics_factory
   use rhyme_samr_factory
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
   samr = samr_factory%generate()
   ic = ic_factory%generate()
   logger = log_factory%generate()

   prim(cid%rho:cid%p) = hy_factory%generate_primitive()

   thermo = th_factory%generate(physics, thid%diatomic)
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

#if NDIM == 1
#elif NDIM == 2
#define JDX ,1
#define JDXEND ,8
#elif NDIM == 3
#define JDX ,1
#define JDXEND ,8
#define KDX ,1
#define KDXEND ,4
#endif

   call tester%expect(samr%levels(0)%boxes(1)%cells(1 JDX KDX, cid%rho:cid%e_tot) .toBe.cons(cid%rho:cid%e_tot) .within.7)
   call tester%expect(samr%levels(0)%boxes(1)%cells(1 JDX KDX, cid%e_tot + 1:NCMP) .toBe.5.23e0.within.7)
   ! TODO: add more tests

   failed = tester%failed()
end function rhyme_drawing_sphere_test
