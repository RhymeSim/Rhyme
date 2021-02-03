program rhyme
   use rhyme_nombre
   use rhyme_chemistry
   use rhyme_units
   use rhyme_hydro_base
   use rhyme_ionisation_equilibrium
   use rhyme_samr
   use rhyme_samr_bc
   use rhyme_cfl
   use rhyme_ideal_gas
   use rhyme_thermo_base
   use rhyme_drawing
   use rhyme_slope_limiter
   use rhyme_irs
   use rhyme_muscl_hancock
   use rhyme_param_parser
   use rhyme_chombo
   use rhyme_initial_condition
   use rhyme_uv_background
   use rhyme_sanity_check
   use rhyme_report
   use rhyme_logger

   implicit none

   type(chemistry_t) :: chemistry
   type(units_t) :: units
   type(samr_t) :: samr
   type(samr_bc_t) :: bc
   type(ionisation_equilibrium_t) :: ie
   type(cfl_t) :: cfl
   type(thermo_base_t) :: thermo
   type(drawing_t) :: draw
   type(irs_t) :: irs
   type(slope_limiter_t) :: sl
   type(muscl_hancock_t) :: mh
   type(mh_workspace_t) :: mhws
   type(chombo_t) :: chombo
   type(initial_condition_t) :: ic
   type(uv_background_t) :: uvb
   type(report_t) :: report
   type(sanity_check_t) :: sc
   type(logger_t) :: logger

   integer :: l, b
   character(len=1024) :: exe_filename, param_file

   ! TODO: use getopt to read flag-based additional command line arguments
   call get_command_argument(0, exe_filename)
   call get_command_argument(1, param_file)

   call logger%init(param_file)

   call logger%begin_section('ツ')
   call logger%log('command line argument:', 'exe', '=', [exe_filename])
   call logger%log('command line argument:', 'param_file', '=', [param_file])
   call logger%log('', 'log_file', '=', [logger%logfile])
   call logger%log('', 'err_file', '=', [logger%errfile])
   call logger%end_section

   call rhyme_nombre_init

   ! Reading parameters and converting them to code units
   call load_params( &
      param_file, chemistry, units, ic, bc, cfl, thermo, uvb, &
      ie, draw, irs, sl, mh, chombo, report, sc, logger)

   call logger%begin_section('init')

   ! TODO: move mh_workspace into muscl_hancock module
   mhws%type = mh%solver_type

   call rhyme_units_init(units, logger)
   call rhyme_chemistry_init(chemistry, units, logger)
   call rhyme_thermo_base_init(thermo, units, logger)
   call rhyme_uv_background_init(uvb, units, logger)
   call rhyme_initial_condition_init(ic, samr, units, logger)
   call rhyme_ionisation_equilibrium_init(ie, units, chemistry, logger)
   call rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, ic%redshift, logger)
   call rhyme_samr_bc_init(bc, thermo, samr, logger)
   call rhyme_irs_init(irs, logger)
   call rhyme_muscl_hancock_init(mh, samr, mhws, logger)
   call rhyme_chombo_init(chombo, samr, logger)

   call rhyme_drawing_init(draw, samr, ic, logger, ie, units, chemistry)

   call rhyme_cfl_init(cfl, thermo, samr, logger)
   call rhyme_sanity_check_init(sc, units, thermo, samr, logger)

   call logger%end_section  ! init

   call logger%begin_section('saving-IC')
   call rhyme_chombo_write_samr_with_nickname('IC', chombo, units, samr)
   call logger%end_section(print_duration=.true.)  ! saving-IC

   call logger%begin_section('initial_report')
   call report%publish(samr, logger)
   call logger%end_section  ! initial_report

   ! Main loop
   do while (.true.)
      call logger%begin_section(samr%levels(0)%iteration)

      samr%levels(0)%dt = rhyme_cfl_time_step(cfl%courant_number, samr)

      call logger%log('', 't', '=', [samr%levels(0)%t])
      call logger%log('', 'dt', '=', [samr%levels(0)%dt])

      call logger%begin_section('bc')
      call rhyme_samr_bc_set_boundaries(bc, samr)
      call logger%end_section(print_duration=.true.)  ! bc

      ! Update structured AMR
      ! Update ghost cells of boxes

      ! Store a snapshot if necessary
      if (modulo(samr%levels(0)%iteration, 10) .eq. 0) then
         call logger%begin_section('save-chombo')
         call rhyme_chombo_write_samr(chombo, units, samr)
         call logger%end_section(print_duration=.true.)  ! save-chombo
      end if

      call logger%begin_section('hydro')
      do l = samr%nlevels - 1, 0, -1
         do b = 1, samr%levels(l)%nboxes
            call rhyme_muscl_hancock_solve( &
               mh, samr%levels(l)%boxes(b), &
               samr%levels(l)%dx, samr%levels(l)%dt, irs, sl, mhws, logger)
         end do
      end do
      call logger%end_section(print_duration=.true.)  ! hydro

      samr%levels(0)%t = samr%levels(0)%t + samr%levels(0)%dt
      samr%levels(0)%iteration = samr%levels(0)%iteration + 1

      if (mod(samr%levels(0)%iteration, report%every) == 0) then
         call logger%begin_section('initial_report')
         call report%publish(samr, logger)
         call logger%end_section(print_duration=.true.)  ! initial_report
      end if

      if (sc%enabled .and. mod(samr%levels(0)%iteration, sc%every) == 0) then
         call logger%begin_section('sanity_check')
         call rhyme_sanity_check_perform(sc, samr, logger)
         call logger%end_section(print_duration=.true.)  ! sanity_check
      end if

      call logger%end_section(print_duration=.true.)  ! samr%levels(0)%iteration
   end do
end program rhyme
