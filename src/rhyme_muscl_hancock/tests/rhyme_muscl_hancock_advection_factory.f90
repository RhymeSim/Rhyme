module rhyme_muscl_hancock_advection_factory
   use rhyme_muscl_hancock_factory
   use rhyme_samr_bc_factory
   use rhyme_samr_factory
   use rhyme_cfl_factory

   implicit none

   real(kind=8), parameter, private :: rho_state_param = 1.2d0
   real(kind=8), parameter, private :: rho_bg_param = 1.d0
   real(kind=8), parameter, private :: v_param = .01d0
   real(kind=8), parameter, private :: p_param = .1d0
   real(kind=8), parameter, private :: gamma_param = 7.d0/5.d0
   real(kind=8), parameter, private :: cs_param = sqrt(gamma_param*p_param/rho_bg_param)

contains

   function muscl_hancock_advection_factory_generate(grid, samr, bc, cfl) result(dt)
      implicit none

      integer, intent(in) :: grid(NDIM)
      type(samr_t), intent(inout) :: samr(NDIM)
      type(samr_bc_t), intent(inout) :: bc(NDIM)
      type(cfl_t), intent(inout) :: cfl(NDIM)

      real(kind=8) :: dt(NDIM)

#if NDIM == 1
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#define JDX
#define KDX
#define V_ARRAY [ 0.d0 ]
#elif NDIM == 2
#define LOOP_J do j = 1, samr(d)%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#define JDX ,j
#define KDX
#define V_ARRAY [ 0.d0, 0.d0 ]
#elif NDIM == 3
#define LOOP_J do j = 1, samr(d)%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K do k = 1, samr(d)%levels(0)%boxes(1)%dims(3)
#define LOOP_K_END end do
#define JDX ,j
#define KDX ,k
#define V_ARRAY [ 0.d0, 0.d0, 0.d0 ]
#endif

      integer :: ghosts(NDIM)
      integer :: nboxes(0:samrid%max_nlevels)
      real(kind=8) :: states(cid%rho:cid%e_tot, NDIM), bg(cid%rho:cid%p, NDIM)
      real(kind=8) :: box_length(NDIM)

      integer :: i JDX KDX, d
      integer :: lambda(NDIM)
      real(kind=8) :: dx(NDIM), v(cid%u:cid%u + NDIM - 1, NDIM)

      nboxes = 0
      nboxes(0) = 1
      ghosts = merge(2, 0, grid > 1)
      box_length = 1d0*real(grid/grid(1), kind=8)

      do d = 1, NDIM
         v(cid%u:cid%u + NDIM - 1, d) = V_ARRAY
         v(cid%u + d - 1, d) = v_param
      end do

      dx = 1d0/grid
      dt = dx/v_param

      do d = 1, NDIM
         cfl(d) = cfl_factory_generate(dt(d)*abs(v_param + cs_param)/dx(d))
      end do

      do d = 1, NDIM
         bc(d)%types = bcid%outflow
         bc(d)%types((d - 1)*2 + 1:(d - 1)*2 + 2) = bcid%periodic
         samr(d) = samr_factory%generate_with( &
                   1, grid, ghosts, nboxes, nboxes, box_length)

         lambda = samr(d)%levels(0)%boxes(1)%dims/4

         call conv_prim_vars_to_cons( &
            rho_state_param, v(cid%u:cid%u + NDIM - 1, d), &
            p_param, states(cid%rho:cid%e_tot, d))
         call conv_prim_vars_to_cons( &
            rho_bg_param, v(cid%u:cid%u + NDIM - 1, d), &
            p_param, bg(cid%rho:cid%e_tot, d))

         LOOP_K
         LOOP_J
         do i = 1, samr(d)%levels(0)%boxes(1)%dims(1)
            select case (d)
            case (samrid%x)
               if (mod(i, lambda(1)) >= lambda(1)/2) then
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = states(cid%rho:cid%e_tot, d)
               else
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = bg(cid%rho:cid%e_tot, d)
               end if
#if NDIM > 1
            case (samrid%y)
               if (mod(j, lambda(2)) >= lambda(2)/2) then
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = states(cid%rho:cid%e_tot, d)
               else
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = bg(cid%rho:cid%e_tot, d)
               end if
#endif
#if NDIM > 2
            case (samrid%z)
               if (mod(k, lambda(3)) >= lambda(3)/2) then
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = states(cid%rho:cid%e_tot, d)
               else
                  samr(d)%levels(0)%boxes(1)%cells( &
                     i JDX KDX, cid%rho:cid%e_tot) = bg(cid%rho:cid%e_tot, d)
               end if
#endif
            end select
         end do
         LOOP_J_END
         LOOP_K_END

      end do
   end function muscl_hancock_advection_factory_generate
end module rhyme_muscl_hancock_advection_factory
