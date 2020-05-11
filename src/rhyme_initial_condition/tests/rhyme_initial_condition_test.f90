logical function rhyme_initial_condition_test() result(failed)
   use rhyme_initial_condition
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ic_tester

   type(initial_condition_t) :: ic

   ic_tester = .describe."initial_condition"

   call ic_tester%expect(icid%simple.toBe.1)
   call ic_tester%expect(icid%snapshot.toBe.2)
   call ic_tester%expect(icid%rhyme.toBe.10)
   call ic_tester%expect(icid%radamesh.toBe.11)
   call ic_tester%expect(icid%unset.toBe.-1)

   call ic_tester%expect(ic%type.toBe.icid%unset)
   call ic_tester%expect(ic%base_grid.toBe.icid%unset)
   call ic_tester%expect(ic%nlevels.toBe.icid%unset)
   call ic_tester%expect(ic%max_nboxes.toBe.0)
   call ic_tester%expect(ic%snapshot_path.toBe.'')
   call ic_tester%expect(ic%redshift.toBe.1d0)

   failed = ic_tester%failed()
end function rhyme_initial_condition_test
