logical function rhyme_drawing_uniform_canvas_test() result(failed)
   use rhyme_drawing
   use rhyme_physics_factory
   use rhyme_samr_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX , j
#define KDX
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX , j
#define KDX , k
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K do k = 1, samr%levels(l)%boxes(b)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(samr_t) :: samr
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8) :: prim(cid%rho:cid%p)
   real(kind=8) :: cons(cid%rho:cid%e_tot)
   real(kind=8) :: bg_prim(NCMP)
   integer :: i JDX KDX, l, b

   tester = .describe."drawing uniform_canvas"

   draw%type = drid%uniform_canvas

   thermo = th_factory%generate(physics, thid%diatomic)
   samr = samr_factory%generate()
   logger = log_factory%generate()

   call rhyme_thermo_base_init(thermo, physics, logger)

   prim = hy_factory%generate_primitive()

   call conv_prim_to_cons(prim, cons)

   bg_prim(cid%rho:cid%p) = prim
   bg_prim(cid%p + 1:NCMP) = 1.23e0

   call rhyme_drawing_uniform_canvas(samr, bg_prim)

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes

         LOOP_K
         LOOP_J
         do i = 1, samr%levels(l)%boxes(b)%dims(1)
            call tester%expect(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho:cid%e_tot) .toBe.cons)
            call tester%expect(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%p + 1:NCMP) .toBe.bg_prim(cid%p + 1:NCMP))
         end do
         LOOP_J_END
         LOOP_K_END

      end do
   end do

   failed = tester%failed()
end function rhyme_drawing_uniform_canvas_test
