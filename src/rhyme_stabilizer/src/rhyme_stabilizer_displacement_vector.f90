submodule(rhyme_stabilizer) displacement_vector_smod
contains
   module function rhyme_stabilizer_displacement_vector(st, samr) result(vec)
      type(stabilizer_t), intent(in) :: st
      type(samr_t), intent(in) :: samr
      real(kind=8) :: vec(NDIM)

      real(kind=8) :: current_center(NDIM)

      current_center = &
         rhyme_samr_weighted_average_of_distances( &
         samr, st%weight, st%weight_power, rhyme_stabilizer_coords, rhyme_stabilizer_weights)

      vec = current_center - st%target_center
   end function rhyme_stabilizer_displacement_vector
end submodule displacement_vector_smod
