module rhyme_irs
   use rhyme_units
   use rhyme_hydro_base
   use rhyme_thermo_base
   use rhyme_logger

   implicit none

   real(kind=8), private :: gm1 = 0.d0
   real(kind=8), private :: gp1 = 0.d0
   real(kind=8), private :: gm1_gp1 = 0.d0
   real(kind=8), private :: gm1_2g = 0.d0
   real(kind=8), private :: gp1_2g = 0.d0
   real(kind=8), private :: g_inv = 0.d0

   type irs_t
      integer :: n_iteration = 10000
      real(kind=8) :: tolerance = 1d-8
   end type irs_t

   interface
      module function rhyme_irs_exec( &
         irs, l_rho, l_v, l_p, l_cs, l_f, r_rho, r_v, r_p, r_cs, r_f, axis) result(p_star)
         type(irs_t), intent(in) :: irs
         real(kind=8), intent(in) :: &
            l_rho, l_v(NDIM), l_p, l_cs, r_rho, r_v(NDIM), r_p, r_cs
         real(kind=8), intent(inout) :: l_f, r_f
         integer, intent(in) :: axis
         real(kind=8) :: p_star
      end function rhyme_irs_exec

      module subroutine rhyme_irs_nonlinear_wave_function( &
         state_rho, state_p, state_cs, p, f, fprime)
         real(kind=8), intent(in) :: state_rho, state_p, state_cs, p
         real(kind=8), intent(inout) :: f, fprime
      end subroutine rhyme_irs_nonlinear_wave_function

      pure module function rhyme_irs_guess_p_star( &
         l_rho, l_v, l_p, l_cs, r_rho, r_v, r_p, r_cs, axis) result(p_star)
         real(kind=8), intent(in) :: l_rho, l_v(NDIM), l_p, l_cs, r_rho, r_v(NDIM), r_p, r_cs
         integer, intent(in) :: axis
         real(kind=8) :: p_star(6)
      end function rhyme_irs_guess_p_star

      module subroutine rhyme_irs_init(irs, thermo, logger)
         implicit none

         type(irs_t), intent(inout) :: irs
         type(thermo_base_t), intent(in) :: thermo
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_irs_init

   end interface
end module rhyme_irs
