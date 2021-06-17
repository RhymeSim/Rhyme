submodule(rhyme_deep_rs) init_smod
contains
   module subroutine rhyme_deep_rs_init(drs, units, logger)
      implicit none

      type(deep_rs_t), intent(inout) :: drs
      type(units_t), intent(in) :: units
      type(logger_t), intent(inout) :: logger

      type(hdf5_util_t) :: h5
      integer :: wshape(2), bshape(2)

      call logger%begin_section('deep_rs')

      call logger%log('Opening model file', '', '@', [drs%path])
      call rhyme_hdf5_util_open(h5, drs%path)

      call rhyme_hdf5_util_read_group_attr(h5, "/", "n_layers", drs%n_layers)
      call logger%log('# of layers', '', '=', [drs%n_layers])

      call logger%log('Allocating weights and biases')

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_0", "weights_shape", wshape)
      call logger%log('1st layer', 'weight shape', '=', wshape)

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_0", "biases_shape", bshape)
      call logger%log('1st layer', 'bias shape', '=', bshape)

      allocate (drs%w1(wshape(1), wshape(2)), drs%b1(bshape(1), bshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_0/weights", drs%w1)
      call logger%log('1st layer weights', '[1:5]', '=', drs%w1(1, :5))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_0/biases", drs%b1)
      call logger%log('1st layer biases', '[1:5]', '=', drs%b1(1, :5))

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_1", "weights_shape", wshape)
      call logger%log('2nd layer', 'weight shape', '=', wshape)

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_1", "biases_shape", bshape)
      call logger%log('2nd layer', 'bias shape', '=', bshape)

      allocate (drs%w2(wshape(1), wshape(2)), drs%b2(bshape(1), bshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_1/weights", drs%w2)
      call logger%log('2nd layer weights', '[1:5]', '=', drs%w2(1, :5))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_1/biases", drs%b2)
      call logger%log('2nd layer biases', '[1:5]', '=', drs%b2(1, :5))

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_2", "weights_shape", wshape)
      call logger%log('3rd layer', 'weight shape', '=', wshape)

      call rhyme_hdf5_util_read_group_1d_array_attr(h5, "/layer_2", "biases_shape", bshape)
      call logger%log('3rd layer', 'bias shape', '=', bshape)

      allocate (drs%w3(wshape(1), wshape(2)), drs%b3(bshape(1), bshape(2)))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_2/weights", drs%w3)
      call logger%log('3rd layer weights', '[1:5]', '=', drs%w3(:5, 1))

      call rhyme_hdf5_util_read_2d_dataset(h5, "/layer_2/biases", drs%b3)
      call logger%log('3rd layer biases', '[1:1]', '=', drs%b3(1, :1))

      drs%rho_conv = real(rhyme_nombre_get_value((1.u. (kilogram/meter**3)) .to. (units%rho)))
      call logger%log('density conversion', '', '=', [drs%rho_conv])

      drs%p_conv = real(rhyme_nombre_get_value((1.u. (kilogram/meter**3*meter**2/second**2)) .to. (units%pressure)))
      call logger%log('pressure conversion', '', '=', [drs%p_conv])

      drs%v_conv = real(rhyme_nombre_get_value((1.u. (meter/second)) .to. (units%velocity)))
      call logger%log('velocity conversion', '', '=', [drs%v_conv])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "drho", drs%drho)
      call logger%log('Δρ', '', '=', [drs%drho])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "dp", drs%dp)
      call logger%log('Δp', '', '=', [drs%dp])

      call rhyme_hdf5_util_read_group_attr(h5, "/", "dv", drs%dv)
      call logger%log('Δv', '', '=', [drs%dv])

      call logger%end_section
   end subroutine rhyme_deep_rs_init
end submodule init_smod
