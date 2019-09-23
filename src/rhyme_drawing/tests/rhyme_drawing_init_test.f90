logical function rhyme_drawing_init_test () result ( failed )
  use rhyme_drawing_factory
  use rhyme_samr_factory
  use rhyme_logger_factory
  use rhyme_nombre_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( drawing_t ) :: draw
  type ( samr_t ) :: samr
  type ( logger_t ) :: logger

  tester = .describe. "drawing_init"

  call rhyme_nombre_init
  samr = samr_factory%generate()
  logger = log_factory%generate()

  draw%shapes => draw%new_shape( drid%sphere )
  draw%shapes%sphere%unit_str = 'km'
  draw%shapes%next => draw%new_shape( drid%sphere )
  draw%shapes%next%sphere%unit_str = 'pc'

  call rhyme_drawing_init( draw, samr, logger )

  call tester%expect( draw%shapes%sphere%unit .toBe. kilo * meter )
  call tester%expect( draw%shapes%next%sphere%unit .toBe. parsec )

  failed = tester%failed()
end function rhyme_drawing_init_test
