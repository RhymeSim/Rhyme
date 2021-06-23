logical function rhyme_drawing_density_power_law_canvas_test() result(failed)
   use rhyme_drawing_factory
   use rhyme_units_factory
   use rhyme_samr_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(drawing_t) :: draw
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(samr_t) :: samr
   type(logger_t) :: logger

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K do k = 1, samr%levels(l)%boxes(b)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

   integer :: l, b, i JDX KDX
   real(kind=8) :: c(NDIM), rho0, r0, r1, power, bg_prim(NCMP), bg(cid%rho:cid%e_tot), rho

   tester = .describe."drawing_density_power_law"

   units = units_factory_generate('SI')
   samr = samr_factory%generate()
   logger = logger_factory_generate('unicode-plotting')
   draw = drawing_factory_generate('blank')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   ! TODO: make parameters random
   c = 0d0
   rho0 = 1.23d4
   r0 = 2.34d0
   r1 = 3.45d0
   power = -2d0
   bg_prim(cid%rho) = 2.34d5
   bg_prim(cid%u:cid%u + NDIM - 1) = 0d0
   bg_prim(cid%e_tot) = 3.45d6
   bg_prim(cid%temp) = 4.56d7
   bg_prim(cid%temp + 1:NCMP) = 5.67d0

   call rhyme_drawing_density_power_law_canvas(samr, units, c, rho0, r0, r1, power, bg_prim, .true.)

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes

         LOOP_K
         LOOP_J
         do i = 1, samr%levels(l)%boxes(b)%dims(1)
            rho = density_power_law([i JDX KDX], rho0, c, r0, r1, power)
            call tester%expect( &
               .notToBeNan.samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho))
            call tester%expect( &
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho) .toBe.rho)

            bg_prim(cid%p) = rho*units%kb%v/units%amu%v*bg_prim(cid%temp)
            call conv_prim_to_cons(bg_prim, bg)

            call tester%expect( &
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho_u:cid%e_tot) &
               .toBe.bg(cid%rho_u:cid%e_tot) .within.7)
            call tester%expect( &
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%e_tot + 1:NCMP) &
               .toBe.bg_prim(cid%p + 1:NCMP))
         end do
         LOOP_J_END
         LOOP_K_END
      end do
   end do

   failed = tester%failed()

contains

   pure function density_power_law(x, rho0, c, r0, r1, p) result(new_rho)
      implicit none

      integer, intent(in) :: x(NDIM)
      real(kind=8), intent(in) :: rho0, c(NDIM), r0, r1, p
      real(kind=8) :: new_rho

      real(kind=8) :: dist

      dist = sqrt(sum((x - c)**2))

      if (dist <= r0) then
         new_rho = rho0
      else
         new_rho = rho0*(dist/r1)**p
      end if
   end function density_power_law
end function rhyme_drawing_density_power_law_canvas_test
