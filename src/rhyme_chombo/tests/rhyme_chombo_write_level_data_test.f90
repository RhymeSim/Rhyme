logical function rhyme_chombo_write_level_data_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: nickname = 'rhyme_chombo_write_level_data'

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch
   type(samr_t) :: samr
   type(units_t) :: units
   type(logger_t) :: logger

   character(len=1024) :: filename = ''
   integer :: l, b, length, bdims(NDIM), offset, lb, ub
   character(len=32) :: level_data_name
   real(kind=4), allocatable :: data(:), expected_data(:)
   type(samr_box_t) :: box

   ch_tester = .describe."chombo write_level_data"

   ch = chombo_factory_generate('empty')
   samr = samr_factory%generate()
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_units_init(units, logger)

   call rhyme_chombo_init(ch, samr, logger)

   ! Crete chombo file
   ch%nickname = nickname
   ch%iteration = samr%levels(0)%iteration
   call rhyme_chombo_filename_generator(ch%prefix, ch%nickname, ch%iteration, filename)

   call rhyme_chombo_create_chombo(ch)
   call rhyme_chombo_write_headers(ch, units, samr)

   do l = 0, samr%nlevels - 1
      call rhyme_chombo_write_level_data(ch, samr%levels(l))
   end do

   call rhyme_hdf5_util_close(ch%file)

   ! Tests
   call rhyme_hdf5_util_open(ch%file, filename)

   do l = 0, samr%nlevels - 1
      write (level_data_name, '(A7,I1,A)') "/level_", l, "/data:datatype=0"

      length = 0
      do b = 1, samr%levels(l)%nboxes
         length = length + product(samr%levels(l)%boxes(b)%dims)
      end do

      allocate (data(NCMP*length))
      allocate (expected_data(NCMP*length))

      call rhyme_hdf5_util_read_1d_dataset(ch%file, level_data_name, data)

      offset = 1
      do b = 1, samr%levels(l)%nboxes
         bdims = samr%levels(l)%boxes(b)%dims
         box = samr%levels(l)%boxes(b)

         ! Rho
         lb = offset + (cid%rho - 1)*product(bdims)
         ub = lb + product(bdims) - 1

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J , 1:bdims(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J , 1:bdims(2)
#define RANGE_K , 1:bdims(3)
#endif

         expected_data(lb:ub) = &
            real( &
            reshape( &
            box%cells(1:bdims(1) RANGE_J RANGE_K, cid%rho), &
            [product(bdims)] &
            ) &
            )

         call ch_tester%expect(data(lb:ub) .toBe.expected_data(lb:ub) .hint.level_data_name)

         ! E_tot
         lb = lb + (cid%e_tot - 1)*product(bdims)
         ub = lb + product(bdims) - 1

         expected_data(lb:ub) = &
            real( &
            reshape( &
            box%cells(1:bdims(1) RANGE_J RANGE_K, cid%e_tot), &
            [product(bdims)] &
            ) &
            )

         call ch_tester%expect(data(lb:ub) .toBe.expected_data(lb:ub) .hint.level_data_name)

         offset = offset + NCMP*product(bdims)
      end do

      deallocate (data)
      deallocate (expected_data)

      ! TODO: Add test for boxes
   end do

   call rhyme_hdf5_util_close(ch%file)

   failed = ch_tester%failed()
end function rhyme_chombo_write_level_data_test
