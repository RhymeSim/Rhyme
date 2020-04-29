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
   pt%elements(ptid%H)%species%symb = 'HII'
   pt%elements(ptid%H)%species%ionized = 1
   pt%elements(ptid%H)%species%RI_A => RI_HII_A
   pt%elements(ptid%H)%species%RI_B => RI_HII_B
   pt%elements(ptid%H)%species%CI => CI_HI

   ! Helium

   pt%elements(ptid%He)%symb = 'He'
   pt%elements(ptid%He)%atomic_number = 2
   pt%elements(ptid%He)%atomic_weight = 4.002602d0.u.atomic_mass_unit

   allocate (pt%elements(ptid%He)%species)
   pt%elements(ptid%He)%species%symb = 'HeII'
   pt%elements(ptid%He)%species%ionized = 1
   pt%elements(ptid%He)%species%RI_A => RI_HeII_A
   pt%elements(ptid%He)%species%RI_B => RI_HeII_B
   pt%elements(ptid%He)%species%CI => CI_HeI

   allocate (pt%elements(ptid%He)%species%next)
   pt%elements(ptid%He)%species%next%prev => pt%elements(ptid%He)%species
   pt%elements(ptid%He)%species%next%symb = 'HeIII'
   pt%elements(ptid%He)%species%next%ionized = 2
   pt%elements(ptid%He)%species%next%RI_A => RI_HeIII_A
   pt%elements(ptid%He)%species%next%RI_B => RI_HeIII_B
   pt%elements(ptid%He)%species%next%CI => CI_HeII

   call logger%end_section

contains
   ! From Hui & Gnedin (1997)

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

   real(kind=8) pure function CI_HI(t)
      real(kind=8), intent(in) :: t
      real(kind=8) :: lambda

      lambda = 2d0*T_H/T
      if (lambda < 140d0) then
         CI_HI = 21.11/(T**1.5)*exp(-0.5*lambda)*(lambda**(-1.089))/(1.+(lambda/0.354)**0.874)**1.101
      else
         CI_HI = 0d0
      end if
   end function CI_HI

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
end subroutine rhyme_periodic_table_init
end submodule init_smod
