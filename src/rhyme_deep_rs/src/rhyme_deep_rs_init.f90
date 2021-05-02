submodule(rhyme_deep_rs) init_smod
contains
   module subroutine rhyme_deep_rs_init(drs, logger)
      implicit none

      type(deep_rs_t), intent(inout) :: drs
      type(logger_t), intent(inout) :: logger

      type(hdf5_util_t) :: h5
      integer :: wshape(2)

      call logger%begin_section('deep_rs')

      call logger%log('Opening model file', '', '@', [drs%path])
      call rhyme_hdf5_util_open(h5, drs%path)

      call rhyme_hdf5_util_read_group_attr(h5, "/", "n_layers", drs%n_layers)
      call logger%log('# of layers', '', '=', [drs%n_layers])

      ! call rhyme_hdf5_util_read_group_attr(h5, "/", "d_norm", drs%d_norm)
      ! call logger%log('Density normalization', '', ':', [drs%d_norm])
      !
      ! call rhyme_hdf5_util_read_group_attr(h5, "/", "p_norm", drs%p_norm)
      ! call logger%log('Pressure normalization', '', ':', [drs%p_norm])
      !
      ! call rhyme_hdf5_util_read_group_attr(h5, "/", "v_norm", drs%v_norm)
      ! call logger%log('Velocity normalization', '', ':', [drs%v_norm])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "drho", drs%drho)
      call logger%log('Δρ', '', '=', [drs%drho])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "dp", drs%dp)
      call logger%log('Δp', '', '=', [drs%dp])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "dv", drs%dv)
      call logger%log('Δv', '', '=', [drs%dv])

      call logger%log('Allocating weights and biases')

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/level_1", "weights_shape", wshape)
      call logger%log('1st layer', 'weight shape', '=', wshape)

      allocate (drs%w1(wshape(1), wshape(2)), drs%b1(wshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/level_1/weights", drs%w1)
      call logger%log('1st layer', '[weights]', '=', drs%w1(1, :5))

      call rhyme_hdf5_util_read_1d_dataset(h5, "/level_1/biases", drs%b1)
      call logger%log('1st layer', '[biases]', '=', drs%b1(:5))

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/level_2", "weights_shape", wshape)
      call logger%log('2nd layer', 'weight shape', '=', wshape)

      allocate (drs%w2(wshape(1), wshape(2)), drs%b2(wshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/level_2/weights", drs%w2)
      call logger%log('2nd layer', '[weights]', '=', drs%w2(1, :5))

      call rhyme_hdf5_util_read_1d_dataset(h5, "/level_2/biases", drs%b2)
      call logger%log('2nd layer', '[biases]', '=', drs%b2(:5))

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/level_3", "weights_shape", wshape)
      call logger%log('3rd layer', 'weight shape', '=', wshape)

      allocate (drs%w3(wshape(1), wshape(2)), drs%b3(wshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/level_3/weights", drs%w3)
      call logger%log('3rd layer', '[weights]', '=', drs%w3(1, :5))

      call rhyme_hdf5_util_read_1d_dataset(h5, "/level_3/biases", drs%b3)
      call logger%log('3rd layer', '[biases]', '=', drs%b3(:5))

      call logger%end_section
   end subroutine rhyme_deep_rs_init
end submodule init_smod
