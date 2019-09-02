logical function rhyme_drawing_uniform_sphere_test () result ( failed )
  use rhyme_drawing
  use rhyme_physics_factory
  use rhyme_samr_factory
  use rhyme_hydro_base_factory
  use rhyme_thermo_base_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: dr_tester

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
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

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape
  type ( physics_t ) :: physics
  type ( samr_t ) :: samr
  type ( thermo_base_t ) :: thermo
  type ( logger_t ) :: logger
  real ( kind=8 ) :: prim( cid%rho:cid%p )
  real ( kind=8 ) :: cons( cid%rho:cid%e_tot )

  integer :: l, b, i JDX KDX
  real ( kind=8 ), parameter :: origin( NDIM ) = 8.d0
  real ( kind=8 ), parameter :: r = 4.d0

  dr_tester = .describe. "drawing uniform_sphere"

  call rhyme_nombre_units_init

  physics = ph_factory%generate()
  samr = samr_factory%generate()
  logger = log_factory%generate()

  prim = hy_factory%generate_primitive()

  thermo = th_factory%generate( physics, thid%diatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )

  call conv_prim_to_cons( prim, cons )

  draw%type = drid%transparent_canvas

  shape => draw%new_shape( drid%sphere )

  shape%sphere%origin = origin
  shape%sphere%r = r
  shape%fill%type = drid%uniform
  shape%fill%colors( cid%rho:cid%p, 1 ) = prim


  call rhyme_drawing_uniform_sphere( samr, shape )


  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes

      LOOP_K
        LOOP_J
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_sphere( [ i JDX KDX ], samr%levels(l)%boxes(b), shape ) ) then
              call dr_tester%expect( &
                samr%levels(l)%boxes(b)%cells( i JDX KDX, cid%rho:cid%e_tot ) &
                .toBe. cons )
            end if
          end do
        LOOP_J_END
      LOOP_K_END

    end do
  end do

  failed = dr_tester%failed()

contains
  pure logical function is_inside_sphere ( p0, box, shape ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: p0( NDIM )
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: shape

    real ( kind=8 ) :: p( NDIM ), r2

    p = real( p0, kind=8 ) / 2**box%level
    r2 = shape%sphere%r**2

    if ( sum( ( p - shape%sphere%origin )**2 ) > r2 ) then
      is_inside = .false.
    else
      is_inside = .true.
    end if
  end function is_inside_sphere
end function rhyme_drawing_uniform_sphere_test
