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
         call rhyme_drawing_uniform_cuboid(samr, shape, logger)

      case (drid%sharp_cuboid)
         call rhyme_drawing_sharp_cuboid(samr, shape, logger)

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

   call logger%end_section
end subroutine rhyme_drawing_init
end submodule init_smod
