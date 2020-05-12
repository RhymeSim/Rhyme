submodule(rhyme_periodic_table) init_smod
contains
module subroutine rhyme_periodic_table_init(pt, logger)
   implicit none

   type(periodic_table_t), intent(inout) :: pt
   type(logger_t), intent(inout) :: logger

   real(kind=8), parameter :: T_H = 157807d0, T_He = 285335d0, T_Hep = 631515d0

   call logger%begin_section('periodic_table')

   call logger%log('Filling elements!')

   ! Hydrogen

   pt%elements(ptid%H)%symb = 'H'
   pt%elements(ptid%H)%atomic_number = 1
   pt%elements(ptid%H)%atomic_weight = 1.00811d0.u.atomic_mass_unit

   allocate (pt%elements(ptid%H)%species)
   pt%elements(ptid%H)%species%symb = 'HI'
   pt%elements(ptid%H)%species%ionized = 0
   pt%elements(ptid%H)%species%RI_A => null()
   pt%elements(ptid%H)%species%RI_B => null()
   pt%elements(ptid%H)%species%CI => null()
   pt%elements(ptid%H)%species%CIE_A => null()
   pt%elements(ptid%H)%species%CIE_B => null()
   pt%elements(ptid%H)%species%IE_A => null()
   pt%elements(ptid%H)%species%IE_B => null()

   allocate (pt%elements(ptid%H)%species%next)
   pt%elements(ptid%H)%species%next%symb = 'HII'
   pt%elements(ptid%H)%species%next%ionized = 1
   pt%elements(ptid%H)%species%next%RI_A => RI_HII_A
   pt%elements(ptid%H)%species%next%RI_B => RI_HII_B
   pt%elements(ptid%H)%species%next%CI => CI_HI
   pt%elements(ptid%H)%species%next%CIE_A => CIE_HI_A
   pt%elements(ptid%H)%species%next%CIE_B => CIE_HI_B
   pt%elements(ptid%H)%species%next%IE_A => IE_HI_A
   pt%elements(ptid%H)%species%next%IE_B => IE_HI_B

   ! Helium

   pt%elements(ptid%He)%symb = 'He'
   pt%elements(ptid%He)%atomic_number = 2
   pt%elements(ptid%He)%atomic_weight = 4.002602d0.u.atomic_mass_unit

   allocate (pt%elements(ptid%He)%species)
   pt%elements(ptid%He)%species%symb = 'HeI'
   pt%elements(ptid%He)%species%ionized = 0
   pt%elements(ptid%He)%species%RI_A => null()
   pt%elements(ptid%He)%species%RI_B => null()
   pt%elements(ptid%He)%species%CI => null()
   pt%elements(ptid%He)%species%CIE_A => null()
   pt%elements(ptid%He)%species%CIE_B => null()
   pt%elements(ptid%He)%species%IE_A => null()
   pt%elements(ptid%He)%species%IE_B => null()

   allocate (pt%elements(ptid%He)%species%next)
   pt%elements(ptid%He)%species%next%symb = 'HeII'
   pt%elements(ptid%He)%species%next%ionized = 1
   pt%elements(ptid%He)%species%next%RI_A => RI_HeII_A
   pt%elements(ptid%He)%species%next%RI_B => RI_HeII_B
   pt%elements(ptid%He)%species%next%CI => CI_HeI
   pt%elements(ptid%He)%species%next%CIE_A => CIE_HeI_A
   pt%elements(ptid%He)%species%next%CIE_B => CIE_HeI_B
   pt%elements(ptid%He)%species%next%IE_A => IE_HeI_A
   pt%elements(ptid%He)%species%next%IE_B => IE_HeI_B

   allocate (pt%elements(ptid%He)%species%next%next)
   pt%elements(ptid%He)%species%next%next%prev => pt%elements(ptid%He)%species
   pt%elements(ptid%He)%species%next%next%symb = 'HeIII'
   pt%elements(ptid%He)%species%next%next%ionized = 2
   pt%elements(ptid%He)%species%next%next%RI_A => RI_HeIII_A
   pt%elements(ptid%He)%species%next%next%RI_B => RI_HeIII_B
   pt%elements(ptid%He)%species%next%next%CI => CI_HeII
   pt%elements(ptid%He)%species%next%next%CIE_A => CIE_HeII_A
   pt%elements(ptid%He)%species%next%next%CIE_B => CIE_HeII_B
   pt%elements(ptid%He)%species%next%next%IE_A => IE_HeII_A
   pt%elements(ptid%He)%species%next%next%IE_B => IE_HeII_B

   call logger%end_section

contains
   ! From Hui & Gnedin (1996)

   real(kind=8) pure function RI_HII_A(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2d0*T_H/T
      RI_HII_A = 1.269d-13*(lambda**1.503)/(1d0 + (lambda/0.522)**0.47)**1.923
   end function RI_HII_A

   real(kind=8) pure function RI_HII_B(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2d0*T_H/T
      RI_HII_B = 2.753d-14*(lambda**1.500)/(1d0 + (lambda/2.740)**0.407)**2.242
   end function RI_HII_B

   real(kind=8) pure function CI_HI(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2d0*T_H/T
      if (lambda < 140d0) then
         CI_HI = 21.11/(T**1.5)*exp(-0.5*lambda)*(lambda**(-1.089))/(1.+(lambda/0.354)**0.874)**1.101
      else
         CI_HI = 0d0
      end if
   end function CI_HI

   real(kind=8) pure function CIE_HI_A(T)
      real(kind=8), intent(in) :: T

      CIE_HI_A = RI_HII_A(T)/(RI_HII_A(T) + CI_HI(T))
   end function CIE_HI_A

   real(kind=8) pure function CIE_HI_B(T)
      real(kind=8), intent(in) :: T

      CIE_HI_B = RI_HII_B(T)/(RI_HII_B(T) + CI_HI(T))
   end function CIE_HI_B

   real(kind=8) pure function IE_HI_A(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne

      IE_HI_A = RI_HII_A(T)/(RI_HII_A(T) + CI_HI(T) + Gamma(1)/ne)
   end function IE_HI_A

   real(kind=8) pure function IE_HI_B(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne

      IE_HI_B = RI_HII_B(T)/(RI_HII_B(T) + CI_HI(T) + Gamma(1)/ne)
   end function IE_HI_B

   real(kind=8) pure function RI_HeII_A(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_He/T
      RI_HeII_A = 3.e-14*lambda**0.654
   end function RI_HeII_A

   real(kind=8) pure function RI_HeII_B(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_He/T
      RI_HeII_B = 1.26e-14*lambda**0.750
   end function RI_HeII_B

   real(kind=8) pure function CI_HeI(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_He/T
      if (lambda < 140.) then
         CI_HeI = 32.38/(T**1.5)*exp(-0.5*lambda)*(lambda**(-1.146))/(1.+(lambda/0.416)**0.987)**1.056
      else
         CI_HeI = 0.
      end if
   end function CI_HeI

   real(kind=8) pure function CIE_HeI_A(T)
      real(kind=8), intent(in) :: T

      CIE_HeI_A = RI_HeII_A(T)/(RI_HeII_A(T) + CI_HeI(T))
   end function CIE_HeI_A

   real(kind=8) pure function CIE_HeI_B(T)
      real(kind=8), intent(in) :: T

      CIE_HeI_B = RI_HeII_B(T)/(RI_HeII_B(T) + CI_HeI(T))
   end function CIE_HeI_B

   real(kind=8) pure function IE_HeI_A(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne
      IE_HeI_A = RI_HeII_A(T)/(RI_HeII_A(T) + CI_HeI(T) + Gamma(1)/ne)
   end function IE_HeI_A

   real(kind=8) pure function IE_HeI_B(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne
      IE_HeI_B = RI_HeII_B(T)/(RI_HeII_B(T) + CI_HeI(T) + Gamma(1)/ne)
   end function IE_HeI_B

   real(kind=8) pure function RI_HeIII_A(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_Hep/T
      RI_HeIII_A = 2.538e-13*(lambda**1.503)/(1.e0 + (lambda/0.522)**0.47)**1.923
   end function RI_HeIII_A

   real(kind=8) pure function RI_HeIII_B(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_Hep/T
      RI_HeIII_B = 2.*2.753e-14*(lambda**1.500)/(1.e0 + (lambda/2.740)**0.407)**2.242
   end function RI_HeIII_B

   real(kind=8) pure function CI_HeII(T)
      real(kind=8), intent(in) :: T
      real(kind=8) :: lambda

      lambda = 2.*T_Hep/T
      if (lambda < 140.) then
         CI_HeII = 19.95/(T**1.5)*exp(-0.5*lambda)*(lambda**(-1.089))/(1.+(lambda/0.553)**0.735)**1.275
      else
         CI_HeII = 0.
      end if
   end function CI_HeII

   real(kind=8) pure function CIE_HeII_A(T)
      real(kind=8), intent(in) :: T

      CIE_HeII_A = &
         ( &
         RI_HeIII_A(T) - CIE_HeI_A(T)*(RI_HeIII_A(T) - CI_HeII(T)) &
         )/( &
         RI_HeII_A(T) + RI_HeIII_A(T) + CI_HeII(T) &
         )
   end function CIE_HeII_A

   real(kind=8) pure function CIE_HeII_B(T)
      real(kind=8), intent(in) :: T

      CIE_HeII_B = &
         ( &
         RI_HeIII_B(T) - CIE_HeI_B(T)*(RI_HeIII_B(T) - CI_HeII(T)) &
         )/( &
         RI_HeII_B(T) + RI_HeIII_B(T) + CI_HeII(T) &
         )
   end function CIE_HeII_B

   real(kind=8) pure function IE_HeII_A(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne

      IE_HeII_A = &
         ( &
         RI_HeIII_A(T) - IE_HeI_A(T, Gamma, ne)*(RI_HeIII_A(T) - CI_HeII(T) - Gamma(1)/ne) &
         )/( &
         RI_HeII_A(T) + RI_HeIII_A(T) + CI_HeII(T) + Gamma(2)/ne &
         )
   end function IE_HeII_A

   real(kind=8) pure function IE_HeII_B(T, Gamma, ne)
      real(kind=8), intent(in) :: T, Gamma(:), ne

      IE_HeII_B = &
         ( &
         RI_HeIII_B(T) - IE_HeI_B(T, Gamma, ne)*(RI_HeIII_B(T) - CI_HeII(T) - Gamma(1)/ne) &
         )/( &
         RI_HeII_B(T) + RI_HeIII_B(T) + CI_HeII(T) + Gamma(2)/ne &
         )
   end function IE_HeII_B
end subroutine rhyme_periodic_table_init
end submodule init_smod
