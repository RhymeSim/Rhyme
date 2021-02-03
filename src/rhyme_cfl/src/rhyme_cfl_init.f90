submodule(rhyme_cfl) init_smod
contains
   module subroutine rhyme_cfl_init(cfl, thermo, samr, logger)
      implicit none

      type(cfl_t), intent(inout) :: cfl
      type(thermo_base_t), intent(in) :: thermo
      type(samr_t), intent(in) :: samr
      type(logger_t), intent(inout) :: logger

      real(kind=8) :: min_cs, max_cs, min_rho, max_rho
      real(kind=8), allocatable :: cs(:), rho(:)

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#define IDX i
#endif

#if NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#define IDX i + samr%levels(0)%boxes(1)%dims(1) * (j - 1)
#endif

#if NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_J_END end do
#define LOOP_K do k = 1, samr%levels(0)%boxes(1)%dims(3)
#define LOOP_K_END end do
#define IDX i + samr%levels(0)%boxes(1)%dims(1) * (j - 1) + product(samr%levels(0)%boxes(1)%dims(1:2)) * (k - 1)
#endif

      integer :: i JDX KDX, i_1d

      call logger%begin_section('cfl')

      call logger%log('state_of_matter', '', '=', [thid%som_names(thermo%state_of_matter)])
      call logger%log('Courant number', '', '=', [cfl%courant_number])

      allocate (cs(product(samr%levels(0)%boxes(1)%dims)))
      allocate (rho(product(samr%levels(0)%boxes(1)%dims)))

      LOOP_K
      LOOP_J
      do i = 1, samr%levels(0)%boxes(1)%dims(1)
         i_1d = IDX
         cs(i_1d) = calc_cs(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho:cid%e_tot))
         rho(i_1d) = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
      end do
      LOOP_J_END
      LOOP_K_END

      min_cs = minval(cs)
      max_cs = maxval(cs)
      min_rho = minval(rho)
      max_rho = maxval(rho)

      if (max_cs > min_cs .and. max_rho > min_rho) then
         call logger%histogram( &
            cs, rho, xdomain=[min_cs, max_cs], ydomain=[min_rho, max_rho], &
            cs_scale=plid%log, normalized=.true., &
            labels=['cs ', 'rho'], resolution=[80, 40])
      end if

      call logger%log('sound speed', '', '=', [min_cs, max_cs])
      call logger%log('density', '', '=', [min_rho, max_rho])

      deallocate (cs, rho)
      call logger%end_section
   end subroutine rhyme_cfl_init
end submodule init_smod
