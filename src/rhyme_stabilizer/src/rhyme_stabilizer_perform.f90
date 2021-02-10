submodule(rhyme_stabilizer) perform_smod
contains
   module subroutine rhyme_stabilizer_perform(st, samr, logger)
      type(stabilizer_t), intent(inout) :: st
      type(samr_t), intent(inout) :: samr
      type(logger_t), intent(inout) :: logger

      real(kind=8) :: disp_vec(NDIM), disp_abs
      real(kind=8) :: current_center(NDIM)
      integer :: shift_vec(NDIM) = 0

      if (samr%levels(0)%iteration < st%next_timestep) then
         call logger%log('Not stabilizing before timestep', st%next_timestep)
         return
      end if

      current_center = &
         rhyme_samr_weighted_average_of_distances( &
         samr, st%weight, st%weight_power, rhyme_stabilizer_coords, rhyme_stabilizer_weights)

      call logger%log('center', '[px]', '=', current_center)
      call logger%log('target center', '[px]', '=', st%target_center)

      disp_vec = st%target_center - current_center
      disp_abs = sqrt(sum(disp_vec**2))

      call logger%log( &
         'Displacement (w.r.t the target point)', disp_abs, '=vector=>', [disp_vec])

      if (all(abs(disp_vec) < 1d0)) then
         call logger%log( &
            'No stabilizing is needed!', '(Below 1 px displacment)', &
            ':', disp_vec)
         return
      end if

      if (abs(disp_abs) < st%tolerance) then
         call logger%log( &
            'No stabilizing is needed!', disp_abs, &
            'is well withing the tolerance range', [st%tolerance])
         return
      end if

      if (any(abs(disp_vec) > st%max_displacement)) then
         disp_vec = disp_vec/maxval(abs(disp_vec))*st%max_displacement
      end if

      shift_vec = nint(disp_vec)
      call logger%log('Shifting', '[px]', '=', shift_vec)

      if (any(shift_vec > 0) .or. any(shift_vec < -1)) then
         shift_vec = merge(shift_vec, 0, shift_vec <= 0)
         shift_vec = merge(shift_vec, -1, shift_vec >= -1)

         call logger%warn('(implementation limitation!) New shifting ', '[px]', '=>', shift_vec)
      end if

      if (any(shift_vec /= 0)) then
         call rhyme_stabilizer_shifting(st, samr, shift_vec)
      end if

      st%next_timestep = samr%levels(0)%iteration + st%min_interval
      call logger%log('next check', '[iteration]', ':', [st%next_timestep])
   end subroutine rhyme_stabilizer_perform
end submodule perform_smod
