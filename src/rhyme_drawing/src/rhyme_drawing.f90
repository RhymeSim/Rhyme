module rhyme_drawing
   ! TODO: Move shapes, fillings and perturbations into separate modules
   use rhyme_physics
   use rhyme_thermo_base
   use rhyme_hydro_base
   use rhyme_initial_condition
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: drawing_indices_t
      integer :: unset = -1, none = -2
      integer :: uniform_canvas = 0, transparent_canvas = 1 ! Canvas modes
      integer :: uniform = 10 ! Filling type
      integer :: add = 30, absolute = 31 ! Modes
      integer :: cuboid = 20, sphere = 21
#if NDIM > 1
      integer :: prism = 22
      integer :: smoothed_slab_2d = 23 ! Shapes
#endif
      integer :: linear = 41, cubic = 42, ramp = 43 ! transition types
      integer :: harmonic = 61
#if NDIM > 1
      integer :: symmetric_decaying = 62 ! perturbation types
#endif
      integer :: x = samrid%x
#if NDIM > 1
      integer :: y = samrid%y
#endif
#if NDIM > 2
      integer :: z = samrid%z
#endif
      integer :: cartesian = 101 ! coordinate types
   end type drawing_indices_t

   type(drawing_indices_t), parameter :: drid = drawing_indices_t()

   type shape_filling_t
      integer :: type = drid%uniform
      real(kind=8) :: colors(cid%rho:cid%p, 2) = 0d0
      integer :: modes(2) = drid%unset
   end type shape_filling_t

   type perturbation_harmonic_t
      real(kind=8) :: A = 1.d0, lambda = 0.d0
      real(kind=8) :: base(cid%rho:cid%p)
   end type perturbation_harmonic_t

#if NDIM > 1
   type perturbation_symmetric_decaying_t
      real(kind=8) :: A = 1.d0, pos = 0.d0, sigma = 1.d0
      real(kind=8) :: base(cid%rho:cid%p)
   end type perturbation_symmetric_decaying_t
#endif

   type perturbation_t
      integer :: type = drid%unset
      integer :: coor_type = drid%unset
      integer :: axis = drid%unset
      type(perturbation_harmonic_t) :: harmonic
#if NDIM > 1
      type(perturbation_symmetric_decaying_t) :: sym_decaying
#endif

      type(perturbation_t), pointer :: next => null()
   end type perturbation_t

   type shape_cuboid_t
      integer :: left_corner(NDIM) = 0 ! in unif of pixels
      integer :: lengths(NDIM) = 0 ! in unif of pixels
   end type shape_cuboid_t

   type shape_sphere_t
      real(kind=8), dimension(NDIM) :: origin = 0d0
      real(kind=8) :: r = 0d0
      real(kind=8) :: sigma = 0d0
      character(len=128) :: unit_str = ''
      type(nombre_unit_t), pointer :: unit => null()
   end type shape_sphere_t

#if NDIM > 1
   type shape_prism_t
      real(kind=8) :: vertices(NDIM, 3) ! in unif of pixels
#if NDIM > 2
      real(kind=8) :: thickness ! in unif of pixels
#endif
   end type shape_prism_t
#endif

#if NDIM > 1
   type shape_smoothed_slab_2d
      real(kind=8) :: pos(2) ! in unit of pixels
      real(kind=8) :: sigma(2) ! in unif of pixels
      integer :: axis = drid%x ! direction of the slab
   end type shape_smoothed_slab_2d
#endif

   type shape_t
      integer :: type = drid%unset

      type(shape_cuboid_t) :: cuboid
      type(shape_sphere_t) :: sphere
#if NDIM > 1
      type(shape_prism_t) :: prism
      type(shape_smoothed_slab_2d) :: slab_2d
#endif

      type(shape_filling_t) :: fill

      type(shape_t), pointer :: next => null()
   end type shape_t

   type drawing_t
      integer :: type = drid%transparent_canvas
      real(kind=8) :: canvas(NCMP)
      type(shape_t), pointer :: shapes => null()
      type(perturbation_t), pointer :: perturbs => null()
      logical :: initialized
   contains
      procedure :: new_shape => rhyme_drawing_new_shape
      procedure :: new_perturb => rhyme_drawing_new_perturb
   end type drawing_t

   interface
      module subroutine rhyme_drawing_init(draw, samr, ic, logger)
         type(drawing_t), intent(inout) :: draw
         type(samr_t), intent(inout) :: samr
         type(initial_condition_t), intent(in) :: ic
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_drawing_init

      module subroutine rhyme_drawing_uniform_canvas(samr, bg_prim)
         type(samr_t), intent(inout) :: samr
         real(kind=8), intent(in) :: bg_prim(NCMP)
      end subroutine rhyme_drawing_uniform_canvas

      module subroutine rhyme_drawing_uniform_cuboid(samr, shape)
         type(samr_t), intent(inout) :: samr
         type(shape_t), intent(in) :: shape
      end subroutine rhyme_drawing_uniform_cuboid

      module subroutine rhyme_drawing_sphere(samr, ic, shape, logger)
         type(samr_t), intent(inout) :: samr
         type(initial_condition_t), intent(in) :: ic
         type(shape_t), intent(in) :: shape
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_drawing_sphere

#if NDIM > 1
      module subroutine rhyme_drawing_uniform_prism(samr, shape)
         type(samr_t), intent(inout) :: samr
         type(shape_t), intent(in) :: shape
      end subroutine rhyme_drawing_uniform_prism

      module subroutine rhyme_drawing_smoothed_slab_2d(samr, shape, logger)
         type(samr_t), intent(inout) :: samr
         type(shape_t), intent(in) :: shape
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_drawing_smoothed_slab_2d
#endif

      module subroutine rhyme_drawing_apply_perturbations(samr, perturbs, logger)
         type(samr_t), intent(inout) :: samr
         type(perturbation_t), intent(in), pointer :: perturbs
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_drawing_apply_perturbations

      module function rhyme_drawing_new_shape(this, shape_type) result(shape)
         class(drawing_t), intent(inout) :: this
         integer, intent(in) :: shape_type
         type(shape_t), pointer :: shape
      end function rhyme_drawing_new_shape

      module function rhyme_drawing_new_perturb(this, perturb_type) result(perturb)
         class(drawing_t), intent(inout) :: this
         integer, intent(in) :: perturb_type
         type(perturbation_t), pointer :: perturb
      end function rhyme_drawing_new_perturb
   end interface
end module rhyme_drawing
