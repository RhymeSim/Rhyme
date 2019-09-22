logical function rhyme_drawing_uniform_cuboid_test () result ( failed )
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
  integer, parameter :: left_corner( NDIM ) = 3
  integer, parameter :: lengths = 12

  dr_tester = .describe. "drawing uniform_canvas"

  call rhyme_nombre_init

  physics = ph_factory%generate()
  samr = samr_factory%generate()
  logger = log_factory%generate()

  prim = hy_factory%generate_primitive()

  thermo = th_factory%generate( physics, thid%diatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )

  call conv_prim_to_cons( prim, cons )

  draw%type = drid%transparent_canvas

  shape => draw%new_shape( drid%cuboid )

  shape%cuboid%left_corner = left_corner
  shape%cuboid%lengths = lengths
  shape%fill%type = drid%uniform
  shape%fill%colors( cid%rho:cid%p, 1 ) = prim

  call rhyme_drawing_uniform_cuboid( samr, shape )

  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes

      LOOP_K
        LOOP_J
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_cuboid( [ i JDX KDX ], samr%levels(l)%boxes(b), shape ) ) then
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

  logical function is_inside_cuboid ( p0, box, shape ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: p0( NDIM )
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: shape

    integer :: le( NDIM ), re( NDIM ), p( NDIM )

    p = p0 / 2**box%level

    le = shape%cuboid%left_corner
    re = shape%cuboid%left_corner + shape%cuboid%lengths - 1

    if ( any( p < le ) .or. any( p >  re ) ) then
      is_inside = .false.
    else
      is_inside = .true.
    end if

  end function is_inside_cuboid
end function rhyme_drawing_uniform_cuboid_test
