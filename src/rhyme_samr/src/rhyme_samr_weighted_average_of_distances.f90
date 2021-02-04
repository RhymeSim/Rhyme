submodule(rhyme_samr) weighted_average_of_distances_smod
contains

#if NDIM == 1
#define JCOLON
#define KCOLON
#define JRANGE
#define KRANGE
#endif
#if NDIM == 2
#define JCOLON , :
#define KCOLON
#define JRANGE , 1:dims(2)
#define KRANGE
#endif
#if NDIM == 3
#define JCOLON , :
#define KCOLON , :
#define JRANGE , 1:dims(2)
#define KRANGE , 1:dims(3)
#endif

   module function rhyme_samr_weighted_average_of_distances( &
      samr, idx, pwr, coords, weights) result(center)
      type(samr_t), intent(in) :: samr
      integer, intent(in) :: idx
      real(kind=8), intent(in) :: pwr
      real(kind=8), dimension(:JCOLON KCOLON, :), intent(in) :: coords
      real(kind=8), dimension(:JCOLON KCOLON), intent(inout) :: weights
      real(kind=8), dimension(NDIM) :: center

      integer :: dims(NDIM)
      real(kind=8) :: sum_weights

      dims = samr%levels(0)%boxes(1)%dims

      weights = samr%levels(0)%boxes(1)%cells(1:dims(1) JRANGE KRANGE, idx)**pwr
      sum_weights = sum(weights)

      center(1) = sum(coords(:JCOLON KCOLON, 1)*weights)/sum_weights
#if NDIM > 1
      center(2) = sum(coords(:JCOLON KCOLON, 2)*weights)/sum_weights
#endif
#if NDIM > 2
      center(3) = sum(coords(:JCOLON KCOLON, 3)*weights)/sum_weights
#endif
   end function rhyme_samr_weighted_average_of_distances
end submodule weighted_average_of_distances_smod
