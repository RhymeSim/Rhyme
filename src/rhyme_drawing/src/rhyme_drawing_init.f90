submodule(rhyme_drawing) init_smod
contains
module subroutine rhyme_drawing_init(draw, samr, ic, logger, ie, physics, chemistry)
   implicit none

   type(drawing_t), intent(inout) :: draw
   type(samr_t), intent(inout) :: samr
   type(initial_condition_t), intent(in) :: ic
   type(logger_t), intent(inout) :: logger
   type(ionisation_equilibrium_t), intent(in), optional :: ie
   type(physics_t), intent(in), optional :: physics
   type(chemistry_t), intent(in), optional :: chemistry

   type(shape_t), pointer :: shape
   integer :: ci, dims(3)
   real(kind=8) :: box_lengths(3)

   call logger%begin_section('drawing')

   ! Canvas
   select case (draw%type)
   case (drid%transparent_canvas)
      call logger%log('canvas', '', '=', ['transparent'])

   case (drid%uniform_canvas)
      call logger%log('canvas', 'uniform, color', '=', draw%canvas)
      call rhyme_drawing_uniform_canvas(samr, draw%canvas)

   case default
      call logger%err('Unknown canvas type!', 'type', '=', [draw%type])

   end select

   ! Shapes
   shape => draw%shapes

   do while (associated(shape))
      select case (shape%type)
      case (drid%cuboid)
         call rhyme_drawing_uniform_cuboid(samr, shape)

      case (drid%sphere)
         shape%sphere%unit => .parse.shape%sphere%unit_str
         if (present(ie) .and. present(physics) .and. present(chemistry)) then
            call rhyme_drawing_sphere(samr, ic, shape, logger, ie, physics, chemistry)
         else
            call rhyme_drawing_sphere(samr, ic, shape, logger)
         end if

#if NDIM > 1
      case (drid%prism)
         call rhyme_drawing_uniform_prism(samr, shape)

      case (drid%smoothed_slab_2d)
         call rhyme_drawing_smoothed_slab_2d(samr, shape, logger)
#endif

      end select

      shape => shape%next
   end do

   call rhyme_drawing_apply_perturbations(samr, draw%perturbs, logger)

   dims = samr%levels(0)%boxes(1)%dims
   box_lengths = samr%box_lengths

#if NDIM > 1
#define IDXX dims(1)/2
#define IDXY :
#define IDXZ :

#if NDIM == 2
#define JDXX , :
#define JDXY , dims(2)/2
#define JDXZ
#define KDXX
#define KDXY
#define KDXZ
#elif NDIM ==3
#define JDXX , :
#define JDXY , dims(2)/2
#define JDXZ , :
#define KDXX ,:
#define KDXY ,:
#define KDXZ , dims(3)/2
#endif

   do ci = 1, NCMP
      call logger%log(cid%labels(ci))
      select case (logger%projection_axis)
      case (lgid%x)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXX JDXX KDXX, ci), &
            [0d0, box_lengths(2)], [0d0, box_lengths(3)], &
            colorscheme=colorschemes(logger%colormap), auto_setup=.true.)
      case (lgid%y)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXY JDXY KDXY, ci), &
            [0d0, box_lengths(1)], [0d0, box_lengths(3)], &
            colorscheme=colorschemes(logger%colormap), auto_setup=.true.)
      case (lgid%z)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXZ JDXZ KDXZ, ci), &
            [0d0, box_lengths(1)], [0d0, box_lengths(2)], &
            colorscheme=colorschemes(logger%colormap), auto_setup=.true.)
      case default
         call logger%err('Unknonw axis!', '', '', [logger%projection_axis])
      end select
   end do
#endif

   call logger%end_section
end subroutine rhyme_drawing_init
end submodule init_smod
