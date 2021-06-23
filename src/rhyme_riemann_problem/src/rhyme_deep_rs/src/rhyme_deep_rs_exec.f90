submodule(rhyme_deep_rs) exec_smod
contains
   pure module function rhyme_deep_rs_exec(drs, r1, p1, r2, p2, dv) result(p)
      type(deep_rs_t), intent(in) :: drs
      real(kind=8), intent(in) :: r1, p1, r2, p2, dv
      real(kind=8) :: p

      real(kind=4) :: input(1, 5), pp(1, 1)

      input(1, :) = &
         [ &
         real((log10((r1/drs%rho_conv)) - drs%drho)/drs%drho, kind=4), &
         real((log10((p1/drs%p_conv)) - drs%dp)/drs%dp, kind=4), &
         real((log10((r2/drs%rho_conv)) - drs%drho)/drs%drho, kind=4), &
         real((log10((p2/drs%p_conv)) - drs%dp)/drs%dp, kind=4), &
         real((dv/drs%v_conv)/drs%dv, kind=4) &
         ]

      pp = &
         matmul( &
         sigmoid( &
         matmul( &
         relu( &
         matmul( &
         input, drs%w1 &
         ) + drs%b1 &
         ), drs%w2 &
         ) + drs%b2 &
         ), drs%w3 &
         ) + drs%b3

      p = 1d1**(drs%dp*real(pp(1, 1), kind=8) + drs%dp)*drs%p_conv
   end function rhyme_deep_rs_exec

   pure elemental real function relu(x)
      real(kind=4), intent(in) :: x
      relu = max(0.0, x)
   end function relu

   pure elemental real function sigmoid(x)
      real(kind=4), intent(in) :: x
      sigmoid = 1.0/(1.0 + exp(-x))
   end function sigmoid
end submodule exec_smod
