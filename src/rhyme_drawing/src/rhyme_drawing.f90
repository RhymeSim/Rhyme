module rhyme_drawing
  ! TODO: Move shapes, transitions, fillings and perturbations into separate modules

  use rhyme_samr
  use rhyme_hydro_base
  use rhyme_ideal_gas
  use rhyme_log

  implicit none

  type drawing_indices_t
    integer :: unset = -1, none = -2
    integer :: uniform_canvas = 0, transparent_canvas = 1 ! Canvas modes
    integer :: uniform = 10 ! Filling type
    integer :: cuboid = 20, sphere = 21, prism = 22 ! Shapes
    integer :: smoothed_slab_2d = 23 ! Shapes
    integer :: linear = 41, cubic = 42, ramp = 43 ! transition types
    integer :: harmonic = 61, symmetric_decaying = 62 ! perturbation types
    integer :: x = hyid%x, y = hyid%y, z = hyid%z ! Cartesian directions
    integer :: xy = 81, xz = 82, yz = 83 ! Cartesian planes
    integer :: xyz = 84 ! Cartesian volume
    integer :: r = 85, theta = 86, phi = 87 ! spherical directions
    integer :: rtheta = 88, rphi = 89, thetaphi = 90 ! spherical planes
    integer :: rthetaphi = 91 ! spherical volume
    integer :: cartesian = 101, cylindrical = 102, spherical = 103 ! coordinate types
  end type drawing_indices_t

  type ( drawing_indices_t ), parameter :: drid = drawing_indices_t ()


  type shape_transition_t
    integer :: type(6) = drid%none ! samrid sides
    real ( kind=8 ) :: sigma(6) = 0.d0 ! samrid sides
    type ( hydro_primitive_t ) :: colors(6, 2) ! samrid sides, begin/end

    integer :: radial_type = drid%none
    real ( kind=8 ) :: radial_sigma = 0.d0
    type ( hydro_primitive_t ) :: radial_colors(2)
  end type shape_transition_t


  type shape_filling_t
    integer :: type = drid%uniform
    type ( hydro_primitive_t ) :: colors(2)
  end type shape_filling_t


  type perturbation_harmonic_t
    real ( kind=8 ) :: A = 1.d0, lambda = 0.d0
    type ( hydro_primitive_t ) :: base
  end type perturbation_harmonic_t


  type perturbation_symmetric_decaying_t
    real ( kind=8 ) :: A = 1.d0, pos = 0.d0, sigma = 1.d0
    type ( hydro_primitive_t ) :: base
  end type perturbation_symmetric_decaying_t


  type perturbation_t
    integer :: type ! perturbation type
    integer :: coor_type = drid%cartesian ! perturbation coordinate type
    integer :: dir = drid%x ! perturbation direction, plane or volume
    type ( perturbation_harmonic_t ) :: harmonic
    type ( perturbation_symmetric_decaying_t ) :: sym_decaying

    type ( perturbation_t ), pointer :: next => null()
  end type perturbation_t


  type shape_cuboid_t
    integer :: left_corner(3) = 0 ! in unif of pixels
    integer :: lengths(3) = 0 ! in unif of pixels
  end type shape_cuboid_t


  type shape_sphere_t
    real ( kind=8 ) :: origin(3) = 0.d0 ! in unif of pixels
    real ( kind=8 ) :: r = 0.d0 ! in unif of pixels
  end type shape_sphere_t


  type shape_prism_t
    real ( kind=8 ) :: vertices(3, 3) ! in unif of pixels
    real ( kind=8 ) :: thickness ! in unif of pixels
  end type shape_prism_t


  type shape_smoothed_slab_2d
    real ( kind=8 ) :: pos(2) ! in unit of pixels
    real ( kind=8 ) :: sigma(2) ! in unif of pixels
    integer :: dir = drid%x ! direction of the slab
  end type shape_smoothed_slab_2d


  type shape_t
    integer :: type = drid%unset ! Shape

    type ( shape_cuboid_t ) :: cuboid
    type ( shape_sphere_t ) :: sphere
    type ( shape_prism_t ) :: prism
    type ( shape_smoothed_slab_2d ) :: slab_2d

    type ( shape_filling_t ) :: fill
    type ( shape_transition_t ) :: trans

    type ( shape_t ), pointer :: next => null()
  end type shape_t


  type drawing_t
    integer :: type = drid%transparent_canvas
    type ( hydro_primitive_t ) :: canvas
    type ( shape_t ), pointer :: shapes => null()
    type ( perturbation_t ), pointer :: perturbs => null()
    logical :: initialized
  contains
    procedure :: new_shape => rhyme_drawing_new_shape
    procedure :: new_perturb => rhyme_drawing_new_perturb
  end type drawing_t

  interface
    module subroutine rhyme_drawing_uniform_canvas ( samr, ig, bg_prim )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( hydro_primitive_t ), intent ( in ) :: bg_prim
    end subroutine rhyme_drawing_uniform_canvas

    module subroutine rhyme_drawing_uniform_cuboid ( samr, ig, shape )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: shape
    end subroutine rhyme_drawing_uniform_cuboid

    module subroutine rhyme_drawing_uniform_sphere ( samr, ig, shape )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: shape
    end subroutine rhyme_drawing_uniform_sphere

    module subroutine rhyme_drawing_uniform_prism ( samr, ig, shape )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: shape
    end subroutine rhyme_drawing_uniform_prism

    module subroutine rhyme_drawing_smoothed_slab_2d ( samr, ig, shape, log )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: shape
      type ( log_t ), intent ( inout ) :: log
    end subroutine rhyme_drawing_smoothed_slab_2d

    module subroutine rhyme_drawing_apply_perturbations ( samr, ig, perturbs, log )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( perturbation_t ), pointer, intent ( in ) :: perturbs
      type ( log_t ), intent ( inout ) :: log
    end subroutine rhyme_drawing_apply_perturbations

    module subroutine rhyme_drawing_apply ( cfg, ig, samr, log )
      class ( drawing_t ), intent ( inout ) :: cfg
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( samr_t ), intent ( inout ) :: samr
      type ( log_t ), intent ( inout ) :: log
    end subroutine rhyme_drawing_apply
  end interface

contains

  function rhyme_drawing_new_shape ( this, shape_type ) result ( shape )
    implicit none

    class ( drawing_t ), intent(inout) :: this
    integer, intent(in) :: shape_type

    type ( shape_t ), pointer :: shape

    shape => this%shapes

    if ( associated ( shape ) ) then
      do while ( associated ( shape%next ) )
        shape => shape%next
      end do

      allocate( shape%next )
      shape => shape%next
    else
      allocate( this%shapes )
      shape => this%shapes
    end if

    shape%type = shape_type

    shape%cuboid%left_corner = 0
    shape%cuboid%lengths = 0

    shape%sphere%origin = 0.d0
    shape%sphere%r = 0.d0

    shape%prism%vertices = 0.d0
    shape%prism%thickness = 0.d0

    shape%slab_2d%pos = 0.d0
    shape%slab_2d%sigma = 0.d0

    shape%fill%type = drid%unset
    shape%fill%colors(1)%w = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]
    shape%fill%colors(2)%w = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]

    shape%trans%type = drid%none
    shape%trans%sigma = 0.d0
  end function rhyme_drawing_new_shape


  function rhyme_drawing_new_perturb ( this, perturb_type ) result ( perturb )
    implicit none

    class ( drawing_t ), intent ( inout ) :: this
    integer, intent ( in ) :: perturb_type

    type ( perturbation_t ), pointer :: perturb

    perturb => this%perturbs

    if ( associated ( perturb ) ) then
      do while ( associated ( perturb%next ) )
        perturb => perturb%next
      end do

      allocate( perturb%next )
      perturb => perturb%next
    else
      allocate( this%perturbs )
      perturb => this%perturbs
    end if

    perturb%type = perturb_type
    perturb%coor_type = drid%unset
    perturb%dir = drid%unset

    perturb%harmonic%A = 1.d0
    perturb%harmonic%lambda = 0.d0
    perturb%harmonic%base%w = 0.d0

    perturb%sym_decaying%A = 1.d0
    perturb%sym_decaying%pos = 0.d0
    perturb%sym_decaying%sigma = 1.d0
  end function rhyme_drawing_new_perturb


end module rhyme_drawing
