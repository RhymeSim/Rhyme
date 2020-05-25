module rhyme_drawing_factory
   use rhyme_drawing

contains
   function drawing_factory_generate(factory_type) result(draw)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(drawing_t) :: draw

      real(kind=8), dimension(NCMP) :: color1, color2
      real(kind=8) :: rho, p

      rho = 1d-20
      p = 1e-9

      color1(cid%rho) = rho
      color1(cid%rho_u:cid%rho_u + NDIM - 1) = 0d0
      color1(cid%e_tot) = p/(5./3.-1)  ! T ~ 1d7
      color1(cid%temp) = p/rho*(1*1.66e-27)/1.38d-23
      color1(cid%ntr_frac_0) = 0d0
#if NDIM > 1
      color1(cid%ntr_frac_1) = 0d0
#endif
#if NDIM > 2
      color1(cid%ntr_frac_2) = 1d0
#endif

      color2(cid%rho) = 1d3*rho
      color2(cid%rho_u:cid%rho_u + NDIM - 1) = 0d0
      color2(cid%e_tot) = p/(5./3.-1)  ! T ~ 1d7
      color2(cid%temp) = p/(1d3*rho)*(1*1.66e-27)/1.38d-23
      color2(cid%ntr_frac_0) = 1d0
#if NDIM > 1
      color2(cid%ntr_frac_1) = 1d0
#endif
#if NDIM > 2
      color2(cid%ntr_frac_2) = 0d0
#endif

      draw%type = drid%uniform_canvas
      draw%canvas(1:NCMP) = color1

      select case (factory_type)
      case ('sphere-SI')
         draw%shapes => draw%new_shape(drid%sphere)
         draw%shapes%sphere%unit_str = 'm'
         draw%shapes%sphere%origin = .5d0
         draw%shapes%sphere%r = .5d0
         draw%shapes%sphere%sigma = .05d0
         draw%shapes%fill%type = drid%uniform
         draw%shapes%fill%modes(1) = drid%absolute
         draw%shapes%fill%colors(1:NCMP, 1) = color1
         draw%shapes%fill%colors(1:NCMP, 2) = color2
      case default
         print *, 'Unknown factory_type!', factory_type
      end select
   end function drawing_factory_generate
end module rhyme_drawing_factory
