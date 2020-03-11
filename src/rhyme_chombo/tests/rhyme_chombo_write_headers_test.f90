logical function rhyme_chombo_write_headers_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch
   type(samr_t) :: samr
   type(physics_t) :: physics
   type(logger_t) :: logger

   character(len=1024), parameter :: nickname = "rhyme_chombo_write_headers"
   character(len=1024) :: filename

   integer :: ndims_read
   character(len=128) :: length_unit, density_unit, time_unit, velocity_unit, &
                         pressure_unit, temperature_unit

   ch_tester = .describe."chombo write_headers"

   ch = ch_factory%generate()
   samr = samr_factory%generate()
   physics = ph_factory%generate()
   logger = log_factory%generate()

   ch%nickname = nickname
   ch%iteration = samr%levels(0)%iteration

   call rhyme_chombo_init(ch, samr, logger)

   call rhyme_chombo_filename_generator(ch, filename)
   call rhyme_chombo_create_chombo(ch)

   call rhyme_chombo_write_headers(ch, physics, samr)

   call rhyme_hdf5_util_close(ch%file)

   call ch_tester%expect(int(ch%level_ids(0:samr%nlevels - 1)) .notToBe.chid%unset)
   call ch_tester%expect(int(ch%level_ids(samr%nlevels:)) .toBe.chid%unset)
   call ch_tester%expect(int(ch%chombo_global_id) .notToBe.chid%unset)

   call rhyme_hdf5_util_open(ch%file, filename)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "SpaceDim", ndims_read)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "time_unit", time_unit)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "length_unit", length_unit)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "density_unit", density_unit)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "velocity_unit", velocity_unit)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "pressure_unit", pressure_unit)
   call rhyme_hdf5_util_read_group_attr(ch%file, "/Chombo_global", "temperature_unit", temperature_unit)

   call ch_tester%expect(ndims_read.toBe.3)
   call ch_tester%expect(density_unit.toBe..printchain.physics%rho)
   call ch_tester%expect(length_unit.toBe..printchain.physics%length)
   call ch_tester%expect(time_unit.toBe..printchain.physics%time)
   call ch_tester%expect(velocity_unit.toBe..printchain.physics%velocity)
   call ch_tester%expect(pressure_unit.toBe..printchain.physics%pressure)
   call ch_tester%expect(temperature_unit.toBe..printchain.physics%temperature)

   call rhyme_hdf5_util_close(ch%file)

   failed = ch_tester%failed()
end function rhyme_chombo_write_headers_test
