submodule(rhyme_initial_condition) rhyme_ic_load_headers_smod
contains
module subroutine rhyme_initial_condition_load_headers(ic, samr)
   implicit none

   type(initial_condition_t), intent(in) :: ic
   type(samr_t), intent(inout) :: samr

   type(chombo_t) :: ch
   integer :: l, prob_domain(3)
   real(kind=8) :: dx(3)
   character(len=16) :: level_name

   call rhyme_hdf5_util_open(ch%file, ic%snapshot_path)

   call rhyme_hdf5_util_read_group_attr(ch%file, '/', 'num_levels', samr%nlevels)
   call rhyme_hdf5_util_read_group_attr(ch%file, '/', 'iteration', samr%levels(0)%iteration)
   call rhyme_hdf5_util_read_group_attr(ch%file, '/', 'time', samr%levels(0)%t)
   call rhyme_hdf5_util_read_group_comp_1d_array_attr(ch%file, 'level_0', &
                                                      'prob_domain', chid%boxes_headers(4:6), prob_domain)

   samr%base_grid = prob_domain(1:NDIM) + 1
   samr%ghost_cells = merge(2, 0, samr%base_grid > 1)

   ! Initialize other variables
   samr%max_nboxes = ic%max_nboxes
   samr%levels%max_nboxes = ic%max_nboxes
   samr%levels%level = [(l, l=0, samrid%max_nlevels)]

   do l = 0, samr%nlevels - 1
      write (level_name, '(A7,I1)') '/level_', l
      call rhyme_hdf5_util_read_group_1d_array_attr(ch%file, trim(level_name), 'dx', dx)
      samr%levels(l)%dx = dx(1:NDIM)

      call rhyme_hdf5_util_read_group_attr(ch%file, trim(level_name), 'ref_ratio', samr%levels(l)%refine_factor)
   end do

   call rhyme_hdf5_util_close(ch%file)
end subroutine rhyme_initial_condition_load_headers
end submodule rhyme_ic_load_headers_smod
