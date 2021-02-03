module rhyme_samr
   use rhyme_hydro_base

   implicit none

#if NDIM == 1
#define COLON_J
#define COLON_K
#define IDS_J
#define IDS_K
#elif NDIM == 2
#define COLON_J ,:
#define COLON_K
#define IDS_J , y = 2, bottom = 3, top = 4
#define IDS_K
#elif NDIM == 3
#define COLON_J ,:
#define COLON_K ,:
#define IDS_J , y = 2, bottom = 3, top = 4
#define IDS_K , z = 3, back = 5, front = 6
#endif

   type, private :: samr_indices_t
      integer :: ghost = -1
      integer :: unset = -10
      integer :: max_nlevels = 23
      integer :: x = 1, left = 1, right = 2 IDS_J IDS_K
      character(len=8) :: side_names(6) = [ &
                          'left  ', 'right ', 'bottom', 'top   ', 'back  ', 'front ']
   end type samr_indices_t

   type(samr_indices_t), parameter :: samrid = samr_indices_t()

   type samr_box_t
      integer :: dims(NDIM) = samrid%unset
      integer :: level = samrid%unset
      integer :: number = samrid%unset
      integer :: left_edge(NDIM) = samrid%unset
      integer :: right_edge(NDIM) = samrid%unset
      integer, allocatable :: flags(:COLON_J COLON_K)
      real(kind=8), allocatable :: cells(:COLON_J COLON_K, :)
   end type samr_box_t

   type samr_level_t
      integer :: level = samrid%unset
      integer :: nboxes = 0
      integer :: max_nboxes = 0
      integer :: iteration = 0
      real(kind=8) :: refine_factor
      real(kind=8) :: t = 0.d0
      real(kind=8) :: dt, dx(NDIM)
      type(samr_box_t), allocatable :: boxes(:)
   end type samr_level_t

   type samr_t
      logical :: initialized = .false.
      integer :: nlevels
      integer :: base_grid(NDIM)
      integer :: ghost_cells(NDIM)
      integer :: max_nboxes(0:samrid%max_nlevels)
      real(kind=8) :: box_lengths(NDIM)
      type(samr_level_t) :: levels(0:samrid%max_nlevels)
   contains
      procedure :: init_box => rhyme_samr_init_box
   end type samr_t

   interface
      module subroutine rhyme_samr_init_box(this, l, b, dims, ledges, redges)
         class(samr_t), intent(inout) :: this
         integer, intent(in) :: l, b, dims(NDIM)
         integer, intent(in) :: ledges(NDIM), redges(NDIM)
      end subroutine rhyme_samr_init_box

      pure module function rhyme_samr_calc_total_energy(samr) result(total_energy)
         type(samr_t), intent(in) :: samr
         real(kind=8) :: total_energy
      end function rhyme_samr_calc_total_energy

      pure module function rhyme_samr_calc_total_mass(samr) result(total_mass)
         type(samr_t), intent(in) :: samr
         real(kind=8) :: total_mass
      end function rhyme_samr_calc_total_mass
   end interface
end module rhyme_samr
