logical function rhyme_drawing_uniform_prism_test() result(failed)
   ! TODO: no point inside the prism!
   use rhyme_drawing
   use rhyme_physics_factory
   use rhyme_samr_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: dr_tester

#if NDIM > 1

#if NDIM == 2
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

   type(drawing_t) :: draw
   type(shape_t), pointer :: shape
   type(physics_t) :: physics
   type(samr_t) :: samr
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8) :: prim(cid%rho:cid%p)
   real(kind=8) :: cons(cid%rho:cid%e_tot)

   integer :: l, b, i JDX KDX
   real(kind=8) :: vertices(NDIM, 3)
#if NDIM > 2
   real(kind=8), parameter :: thickness = 2
#endif

#endif

   dr_tester = .describe."drawing uniform_prism"

#if NDIM > 1

#if NDIM == 2
   vertices = reshape([1, 1, 5, 12, 13, 8], [NDIM, 3], order=[2, 1])
#elif NDIM == 3
   vertices = reshape([1, 1, 1, 5, 12, 4, 13, 8, 10], [NDIM, 3], order=[2, 1])
#endif

   samr = samr_factory%generate()
   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_physics_init(physics, logger)

   prim = hy_factory%generate_primitive()

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)

   call conv_prim_to_cons(prim, cons)

   draw%type = drid%transparent_canvas

   shape => draw%new_shape(drid%prism)

   shape%prism%vertices = vertices
#if NDIM > 2
   shape%prism%thickness = thickness
#endif
   shape%fill%type = drid%uniform
   shape%fill%colors(cid%rho:cid%p, 1) = prim
   shape%fill%colors(cid%e_tot + 1:NCMP, 1) = 3.45d0

   call rhyme_drawing_uniform_prism(samr, shape)

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes

         LOOP_K
         LOOP_J
         do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if (is_inside_prism([i JDX KDX], samr%levels(l)%boxes(b), shape)) then
               call dr_tester%expect( &
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho:cid%e_tot) &
                  .toBe.cons)
               call dr_tester%expect( &
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%e_tot + 1:NCMP) &
                  .toBe.3.45d0)
            end if
         end do
         LOOP_J_END
         LOOP_K_END

      end do
   end do

#endif

   failed = dr_tester%failed()

#if NDIM > 1

contains

   pure logical function is_inside_prism(point, box, shape) result(is_inside)
      implicit none

      integer, intent(in) :: point(NDIM)
      type(samr_box_t), intent(in) :: box
      type(shape_t), intent(in) :: shape

      real(kind=8), parameter :: pi = 3.1415926535897932_8

      real(kind=8) :: p0(NDIM), vec_p0(NDIM), len_vec_p0
      real(kind=8) :: proj_p(NDIM)
      real(kind=8) :: p_1(NDIM), len_p_1, p_2(NDIM), len_p_2, p_3(NDIM), len_p_3
      real(kind=8) :: theta

#if NDIM > 2
      real(kind=8) :: a(NDIM), b(NDIM), n(NDIM), len_n, dis
#endif

      p0 = real(point, kind=8)/2**box%level
      vec_p0 = p0 - shape%prism%vertices(:, 1)
      len_vec_p0 = sqrt(sum(vec_p0**2))

#if NDIM > 2
      a = shape%prism%vertices(:, 2) - shape%prism%vertices(:, 1)
      b = shape%prism%vertices(:, 3) - shape%prism%vertices(:, 1)

      n(1) = a(2)*b(3) - a(3)*b(2)
      n(2) = a(3)*b(1) - a(1)*b(3)
      n(3) = a(1)*b(2) - a(2)*b(1)

      len_n = sqrt(sum(n**2))

      theta = acos(sum(n*vec_p0)/(len_n*len_vec_p0))
      dis = len_vec_p0*sin(theta - pi/2)

      if (dis > shape%prism%thickness/2) then
         is_inside = .false.
         return
      end if

      proj_p = p0 - dis*n/len_n
#else
      proj_p = vec_p0
#endif

      p_1 = proj_p - shape%prism%vertices(:, 1)
      len_p_1 = sqrt(sum(p_1**2))
      p_2 = proj_p - shape%prism%vertices(:, 2)
      len_p_2 = sqrt(sum(p_2**2))
      p_3 = proj_p - shape%prism%vertices(:, 3)
      len_p_3 = sqrt(sum(p_3**2))

      theta = acos(sum(p_1*p_2)/(len_p_1*len_p_2))
      theta = theta + acos(sum(p_1*p_3)/(len_p_1*len_p_3))
      theta = theta + acos(sum(p_2*p_3)/(len_p_2*len_p_3))

      if (abs(theta - 2*pi) > epsilon(0.e0)) then
         is_inside = .false.
      else
         is_inside = .true.
      end if
   end function is_inside_prism
#endif
end function rhyme_drawing_uniform_prism_test
