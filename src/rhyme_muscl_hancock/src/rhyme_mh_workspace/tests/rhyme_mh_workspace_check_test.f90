logical function rhyme_mh_workspace_check_test() result(failed)
   use rhyme_mh_workspace_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(mh_workspace_t) :: ws
   type(samr_t) :: samr
   type(logger_t) :: logger
   integer :: l, b, arr_size, n_hydro_comp
   integer :: lb(NDIM + 1), ub(NDIM + 1), lbws(NDIM + 2), ubws(NDIM + 2), dims(NDIM)

   tester = .describe."mhws_check"

   samr = samr_factory%generate()
   logger = logger_factory_generate('default')

   call rhyme_mh_workspace_init(ws, samr, logger)

   n_hydro_comp = cid%e_tot - cid%rho + 1

   do l = 0, samr%nlevels
      do b = 1, samr%levels(l)%nboxes
         ! Check memory_intensive
         ws%type = mhwsid%memory_intensive
         call rhyme_mh_workspace_check(ws, samr%levels(l)%boxes(b))

         lb = lbound(samr%levels(l)%boxes(b)%cells)
         ub = ubound(samr%levels(l)%boxes(b)%cells)

         lbws = lbound(ws%levels(l)%boxes(b)%ul)
         ubws = ubound(ws%levels(l)%boxes(b)%ul)

         dims = ub(1:NDIM) - lb(1:NDIM) + 1
         arr_size = product(dims)*n_hydro_comp

         call tester%expect(allocated(ws%levels(l)%boxes(b)%ul) .toBe..true..hint.'allocated: ul')
         call tester%expect(allocated(ws%levels(l)%boxes(b)%ur) .toBe..true..hint.'allocated: ur')
         call tester%expect(allocated(ws%levels(l)%boxes(b)%fr) .toBe..true..hint.'allocated: fr')
         call tester%expect(size(ws%levels(l)%boxes(b)%ul) .toBe.arr_size*NDIM.hint.'size: ul')
         call tester%expect(size(ws%levels(l)%boxes(b)%ur) .toBe.arr_size*NDIM.hint.'size: ur')
         call tester%expect(size(ws%levels(l)%boxes(b)%fr) .toBe.arr_size*NDIM.hint.'size: fr')
         call tester%expect(lb(1:NDIM) .toBe.lbws(1:NDIM) .hint.'lb( 1:NDIM )')
         call tester%expect(lbws(NDIM + 1) .toBe.cid%rho.hint.'lbws( NDIM+1 )')
         call tester%expect(lbws(NDIM + 2) .toBe.1.hint.'lbws( NDIM+2 )')
         call tester%expect(ub(1:NDIM) .toBe.ubws(1:NDIM) .hint.'ub( 1:NDIM )')
         call tester%expect(ubws(NDIM + 1) .toBe.cid%e_tot.hint.'ubws( NDIM+1 )')
         call tester%expect(ubws(NDIM + 2) .toBe.NDIM.hint.'ubws( NDIM+2 )')

         ! Check cpu_intensive
         ws%type = mhwsid%cpu_intensive
         call rhyme_mh_workspace_check(ws, samr%levels(l)%boxes(b))

         lb = lbound(ws%levels(l)%boxes(b)%u)
         ub = ubound(ws%levels(l)%boxes(b)%u)

         dims = samr%levels(l)%boxes(b)%dims
         arr_size = product(dims)*n_hydro_comp

         call tester%expect(allocated(ws%levels(l)%boxes(b)%u) .toBe..true..hint.'allocated: u')
         call tester%expect(size(ws%levels(l)%boxes(b)%u) .toBe.arr_size.hint.'size: u')
         call tester%expect(lb(1:NDIM) .toBe.1.hint.'u_lb')
         call tester%expect(lb(NDIM + 1) .toBe.cid%rho.hint.'u_lb (hydro components)')
         call tester%expect(ub(1:NDIM) .toBe.dims.hint.'u_ub')
         call tester%expect(ub(NDIM + 1) .toBe.cid%e_tot.hint.'u_ub (hydro components)')
      end do
   end do

   failed = tester%failed()
end function rhyme_mh_workspace_check_test
