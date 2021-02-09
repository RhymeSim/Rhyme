submodule(rhyme_stabilizer) init_smod
contains
   module subroutine rhyme_stabilizer_init(st, samr, logger)
      implicit none

      type(stabilizer_t), intent(inout) :: st
      type(samr_t), intent(in) :: samr
      type(logger_t), intent(inout) :: logger

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#define ALLOC_J
#define ALLOC_K
#endif
#if NDIM == 2
#define JDX , j
#define KDX
#define LOOP_J do j = 1, dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#define ALLOC_J , dims(2)
#define ALLOC_K
#endif
#if NDIM == 3
#define JDX , j
#define KDX , k
#define LOOP_J do j = 1, dims(2)
#define LOOP_K do k = 1, dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#define ALLOC_J , dims(2)
#define ALLOC_K , dims(3)
#endif

      character(len=32) :: weight_str
      integer, dimension(NDIM) :: dims
      integer :: i JDX KDX

      call logger%begin_section('stabilizer')

      if (st%enabled) then
         dims = samr%levels(0)%boxes(1)%dims

         call logger%log( &
            'extrapolation type', '', ':', &
            [trim(stid%extrapolation_names(st%extrapolation_type))])

         write (weight_str, '(A,A1,F4.2)') trim(cid%labels(st%weight)), '^', st%weight_power
         call logger%log('weight', '', ':', [trim(weight_str)])

         call logger%log('Max displacement', '', ':', [st%max_displacement])

         call logger%log('Setting up coordinates array')
         allocate (rhyme_stabilizer_coords(dims(1) ALLOC_J ALLOC_K, NDIM))
         allocate (rhyme_stabilizer_weights(dims(1) ALLOC_J ALLOC_K))

         LOOP_K
         LOOP_J
         do i = 1, dims(1)
            rhyme_stabilizer_coords(i JDX KDX, :) = [i JDX KDX]
         end do
         LOOP_J_END
         LOOP_K_END

         if (st%initialize_target) then
            call logger%log('Initializing the target center...')
            st%target_center = &
               rhyme_samr_weighted_average_of_distances( &
               samr, st%weight, st%weight_power, rhyme_stabilizer_coords, rhyme_stabilizer_weights)
         end if

         call logger%log('target center', '[px]', '=', st%target_center)
      else
         call logger%log('disabled')
      end if

      call logger%end_section
   end subroutine rhyme_stabilizer_init
end submodule init_smod
