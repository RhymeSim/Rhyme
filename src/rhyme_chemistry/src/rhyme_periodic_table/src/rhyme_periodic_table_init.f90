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

      pt%elements(1)%symb = 'H'
      pt%elements(1)%atomic_number = 1
      pt%elements(1)%atomic_weight = 1.00811d0
      pt%elements(1)%nspecies = 1
      pt%elements(1)%ne => ne_H

      allocate (pt%elements(1)%species(pt%elements(1)%nspecies))

      pt%elements(1)%species(1)%symb = 'HII'
      pt%elements(1)%species(1)%ionized = 1
      pt%elements(1)%species(1)%RI_A => RI_HII_A
      pt%elements(1)%species(1)%RI_B => RI_HII_B
      pt%elements(1)%species(1)%CI => CI_HI
      pt%elements(1)%species(1)%CIE_A => CIE_HI_A
      pt%elements(1)%species(1)%CIE_B => CIE_HI_B
      pt%elements(1)%species(1)%CPIE_A => CPIE_HI_A
      pt%elements(1)%species(1)%CPIE_B => CPIE_HI_B
      pt%elements(1)%species(1)%ne => ne_HII
      pt%elements(1)%species(1)%f => f_HII

      ! Helium

      pt%elements(2)%symb = 'He'
      pt%elements(2)%atomic_number = 2
      pt%elements(2)%atomic_weight = 4.002602d0
      pt%elements(2)%nspecies = 2
      pt%elements(2)%ne => ne_He

      allocate (pt%elements(2)%species(pt%elements(2)%nspecies))

      pt%elements(2)%species(1)%symb = 'HeII'
      pt%elements(2)%species(1)%ionized = 1
      pt%elements(2)%species(1)%RI_A => RI_HeII_A
      pt%elements(2)%species(1)%RI_B => RI_HeII_B
      pt%elements(2)%species(1)%CI => CI_HeI
      pt%elements(2)%species(1)%CIE_A => CIE_HeI_A
      pt%elements(2)%species(1)%CIE_B => CIE_HeI_B
      pt%elements(2)%species(1)%CPIE_A => CPIE_HeI_A
      pt%elements(2)%species(1)%CPIE_B => CPIE_HeI_B
      pt%elements(2)%species(1)%ne => ne_HeII
      pt%elements(2)%species(1)%f => f_HeII

      pt%elements(2)%species(2)%symb = 'HeIII'
      pt%elements(2)%species(2)%ionized = 2
      pt%elements(2)%species(2)%RI_A => RI_HeIII_A
      pt%elements(2)%species(2)%RI_B => RI_HeIII_B
      pt%elements(2)%species(2)%CI => CI_HeII
      pt%elements(2)%species(2)%CIE_A => CIE_HeII_A
      pt%elements(2)%species(2)%CIE_B => CIE_HeII_B
      pt%elements(2)%species(2)%CPIE_A => CPIE_HeII_A
      pt%elements(2)%species(2)%CPIE_B => CPIE_HeII_B
      pt%elements(2)%species(2)%ne => ne_HeIII
      pt%elements(2)%species(2)%f => f_HeIII

      call logger%end_section

   contains
      ! Hydrogen and helium coefficients from Hui & Gnedin (1996)

      ! Hydrogen
      real(kind=8) pure function ne_H(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)

         ne = 1 - ntr_frac(1)
      end function ne_H

      real(kind=8) pure function ne_HII(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)

         ne = 1 - ntr_frac(1)
      end function ne_HII

      real(kind=8) pure function f_HII(ntr_frac) result(f)
         real(kind=8), intent(in) :: ntr_frac(:)

         f = 1 - ntr_frac(1)
      end function f_HII

      real(kind=8) pure function RI_HII_A(T)
         real(kind=8), intent(in) :: T
         real(kind=8) :: lambda

         lambda = 2*T_H/T
         RI_HII_A = 1.269d-13*(lambda**1.503)/(1d0 + (lambda/0.522)**0.47)**1.923
      end function RI_HII_A

      real(kind=8) pure function RI_HII_B(T)
         real(kind=8), intent(in) :: T
         real(kind=8) :: lambda

         lambda = 2*T_H/T
         RI_HII_B = 2.753d-14*(lambda**1.500)/(1d0 + (lambda/2.740)**0.407)**2.242
      end function RI_HII_B

      real(kind=8) pure function CI_HI(T)
         real(kind=8), intent(in) :: T
         real(kind=8) :: lambda

         lambda = 2*T_H/T
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

      real(kind=8) pure function CPIE_HI_A(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne

         CPIE_HI_A = RI_HII_A(T)/(RI_HII_A(T) + CI_HI(T) + Gamma_phot(1)/ne)
      end function CPIE_HI_A

      real(kind=8) pure function CPIE_HI_B(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne

         CPIE_HI_B = RI_HII_B(T)/(RI_HII_B(T) + CI_HI(T) + Gamma_phot(1)/ne)
      end function CPIE_HI_B

      ! Helium

      real(kind=8) pure function ne_He(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)  ! HeII HeIII

         ne = 2 - 2*ntr_frac(1) - ntr_frac(2)
      end function ne_He

      real(kind=8) pure function ne_HeII(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)  ! HeII HeIII

         ne = 1 - ntr_frac(1)
      end function ne_HeII

      real(kind=8) pure function f_HeII(ntr_frac) result(f)
         real(kind=8), intent(in) :: ntr_frac(:)  ! HeII HeIII

         f = ntr_frac(2)
      end function f_HeII

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

      real(kind=8) pure function CPIE_HeI_A(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne
         CPIE_HeI_A = RI_HeII_A(T)/(RI_HeII_A(T) + CI_HeI(T) + Gamma_phot(2)/ne)
      end function CPIE_HeI_A

      real(kind=8) pure function CPIE_HeI_B(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne
         CPIE_HeI_B = RI_HeII_B(T)/(RI_HeII_B(T) + CI_HeI(T) + Gamma_phot(2)/ne)
      end function CPIE_HeI_B

      real(kind=8) pure function ne_HeIII(ntr_frac) result(ne)
         real(kind=8), intent(in) :: ntr_frac(:)

         ne = 1 - ntr_frac(1) - ntr_frac(2)
      end function ne_HeIII

      real(kind=8) pure function f_HeIII(ntr_frac) result(f)
         real(kind=8), intent(in) :: ntr_frac(:)  ! HeII HeIII

         f = 1 - ntr_frac(1) - ntr_frac(2)
      end function f_HeIII

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

      real(kind=8) pure function CPIE_HeII_A(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne ! Assuming Gamma_phot(HI, HeI, HeII)

         CPIE_HeII_A = &
            ( &
            RI_HeIII_A(T) - CPIE_HeI_A(T, Gamma_phot, ne)*(RI_HeIII_A(T) - CI_HeII(T) - Gamma_phot(2)/ne) &
            )/( &
            RI_HeII_A(T) + RI_HeIII_A(T) + CI_HeII(T) + Gamma_phot(3)/ne &
            )
      end function CPIE_HeII_A

      real(kind=8) pure function CPIE_HeII_B(T, Gamma_phot, ne)
         real(kind=8), intent(in) :: T, Gamma_phot(:), ne ! Assuming Gamma_phot(HI, HeI, HeII)

         CPIE_HeII_B = &
            ( &
            RI_HeIII_B(T) - CPIE_HeI_B(T, Gamma_phot, ne)*(RI_HeIII_B(T) - CI_HeII(T) - Gamma_phot(2)/ne) &
            )/( &
            RI_HeII_B(T) + RI_HeIII_B(T) + CI_HeII(T) + Gamma_phot(3)/ne &
            )
      end function CPIE_HeII_B
   end subroutine rhyme_periodic_table_init
end submodule init_smod
