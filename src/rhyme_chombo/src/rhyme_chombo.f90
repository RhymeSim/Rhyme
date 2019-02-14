module rhyme_chombo
  use rhyme_hdf5_util
  use rhyme_samr

  implicit none

  type rhyme_chombo_indices_t
    integer :: unset = -1
  end type rhyme_chombo_indices_t

  type ( rhyme_chombo_indices_t ), parameter :: chid = rhyme_chombo_indices_t ()

  type, extends ( rhyme_hdf5_util_t ) :: chombo_t
    integer :: num_levels = chid%unset
    integer :: num_components = chid%unset
    integer ( hid_t ) :: chombo_global_id
    integer ( hid_t ) :: level_ids(0:23) = chid%unset
    character ( len=1024 ) :: prefix = " "
    character ( len=1024 ) :: nickname = " "
  contains
    procedure :: init_with => rhyme_chombo_init_with
    procedure :: write_samr => rhyme_chombo_write_samr
    procedure :: write_level_data => rhyme_chombo_write_level_data
    procedure :: filename_generator => rhyme_chombo_filename_generator
  end type chombo_t

contains


  subroutine rhyme_chombo_init_with ( this, prefix, nickname )
    implicit none

    class ( chombo_t ), intent ( inout ) :: this
    character ( len=1024 ), intent ( in ) :: prefix, nickname

    this%prefix = prefix
    this%nickname = nickname

  end subroutine rhyme_chombo_init_with


  subroutine rhyme_chombo_filename_generator ( this, iteration, filename )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    integer, intent ( in ) :: iteration
    character ( len=1024 ), intent ( out ) :: filename

    character ( len=8 ) :: itr_str

    filename = " "

    if ( len_trim( this%prefix ) > 0 ) then
      filename = trim(filename) // trim(this%prefix) // '/'
    end if

    if ( len_trim( this%nickname ) > 0 ) then
      filename = trim(filename) // trim(this%nickname) // "-"
    end if

    write ( itr_str, "(I0.5)" ) iteration

    filename = trim(filename) // trim(itr_str) // ".chombo.h5"
  end subroutine rhyme_chombo_filename_generator


  subroutine rhyme_chombo_write_samr ( this, samr )
    implicit none

    class ( chombo_t ), intent (inout) :: this
    type ( samr_t ), intent (in) :: samr

    integer :: i, ndims, l
    character ( len=16 ) :: level_name
    character ( len=1024 ) :: filename


    if ( this%initialized ) return

    call this%filename_generator ( samr%levels(0)%iteration, filename )
    call this%create ( filename )

    do i = 0, samr%nlevels - 1
      write ( level_name, '(A7,I1)') "/level_", i
      call this%create_group ( level_name, this%level_ids(i) )
      call this%write_group_1d_array_attr ( level_name, "dx", samr%levels(i)%dx )
      call this%write_group_attr ( level_name, "ref_ratio", samr%levels(i)%refine_factor )
    end do

    call this%write_group_comp_1d_array_attr ( "/level_0", "prob_domain", &
      [ "lo_i", "lo_j", "lo_k", "hi_i", "hi_j", "hi_k" ], &
      [ 0, 0, 0, samr%base_grid(1)-1, samr%base_grid(2)-1, samr%base_grid(3)-1 ] )

    call this%write_group_1d_array_attr ( "/", "ProblemDomain", samr%base_grid )
    call this%write_group_attr ( "/", "num_levels", samr%nlevels )
    call this%write_group_attr ( "/", "num_components", 5 )
    call this%write_group_attr ( "/", "component_0", "rho" )
    call this%write_group_attr ( "/", "component_1", "rho_u" )
    call this%write_group_attr ( "/", "component_2", "rho_v" )
    call this%write_group_attr ( "/", "component_3", "rho_w" )
    call this%write_group_attr ( "/", "component_4", "e_tot" )
    call this%write_group_attr ( "/", "iteration", samr%levels(0)%iteration )
    call this%write_group_attr ( "/", "time", samr%levels(0)%t )

    call this%create_group ( "/chombo_global", this%chombo_global_id )

    ndims = size ( samr%base_grid ) - sum ( samr%base_grid * merge ( 1, 0, samr%base_grid <= 1 ) )
    call this%write_group_attr ( "/chombo_global", "SpaceDim", ndims )

    do l = 0, samr%nlevels - 1
      call this%write_level_data ( samr%levels(l) )
    end do

    call this%close
  end subroutine rhyme_chombo_write_samr


  subroutine rhyme_chombo_write_level_data ( this, level )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    type ( samr_level_t ), intent ( in ) :: level

    integer :: b, var, lb, ub, offset = 1, length = 0, dim1d
    real ( kind=8 ), allocatable :: data(:)
    integer, allocatable :: boxes(:,:)

    do b = 1, level%nboxes
      length = length + product( level%boxes(b)%dims ) * 5
    end do

    allocate ( data( length ) )
    allocate ( boxes( length, 6 ) )

    do b = 1, level%nboxes
      dim1d = product( level%boxes(b)%dims )

      do var = hyid%rho, hyid%e_tot
        lb = offset + (var - 1) * dim1d
        ub = lb + dim1d
        ! TODO: Structure of array instead of AoS is needed -> refactoring
        ! data ( lb:ub ) = reshape ( level%boxes%hydro%u(var) )
      end do
    end do

  end subroutine rhyme_chombo_write_level_data

end module rhyme_chombo
