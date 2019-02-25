module rhyme_chombo
  use rhyme_hdf5_util
  use rhyme_samr
  use rhyme_log

  implicit none

  type rhyme_chombo_indices_t
    integer :: unset = h5id%unset
  end type rhyme_chombo_indices_t

  type ( rhyme_chombo_indices_t ), parameter :: chid = rhyme_chombo_indices_t ()

  type, extends ( rhyme_hdf5_util_t ) :: chombo_t
    logical :: is_opened = .false.
    integer :: num_levels = chid%unset
    integer :: num_components = chid%unset
    integer :: iteration = chid%unset
    integer ( hid_t ) :: chombo_global_id = chid%unset
    integer ( hid_t ) :: level_ids(0:23) = chid%unset
    character ( len=1024 ) :: prefix = " "
    character ( len=1024 ) :: nickname = " "
  contains
    procedure :: init_with => rhyme_chombo_init_with
    procedure :: init => rhyme_chombo_init
    procedure :: create_chombo => rhyme_chombo_create_chombo
    procedure :: write_headers => rhyme_chombo_write_headers
    procedure :: write_samr => rhyme_chombo_write_samr
    procedure :: write_level_data => rhyme_chombo_write_level_data
    procedure :: filename_generator => rhyme_chombo_filename_generator
  end type chombo_t

contains


  subroutine rhyme_chombo_init_with ( this, prefix, nickname, log )
    implicit none

    class ( chombo_t ), intent ( inout ) :: this
    character ( len=1024 ), intent ( in ) :: prefix, nickname
    type ( log_t ), intent ( inout ) :: log

    this%prefix = prefix
    this%nickname = nickname

    call this%init( log )
  end subroutine rhyme_chombo_init_with


  subroutine rhyme_chombo_init ( this, log )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    type ( log_t ), intent ( inout ) :: log

    logical :: ex

    inquire ( file=trim(this%prefix)//"/.", exist=ex )

    if ( .not. ex ) then
      call log%warn( trim(this%prefix)//' does not exist!')
      call execute_command_line('mkdir -p '//trim(this%prefix) )
      call log%write( trim(this%prefix)//' has been created' )
    end if
  end subroutine rhyme_chombo_init


  subroutine rhyme_chombo_filename_generator ( this, filename )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    character ( len=1024 ), intent ( out ) :: filename

    character ( len=8 ) :: itr_str

    filename = " "

    if ( len_trim( this%prefix ) > 0 ) then
      filename = trim(filename) // trim(this%prefix) // '/'
    end if

    if ( len_trim( this%nickname ) > 0 ) then
      filename = trim(filename) // trim(this%nickname) // "-"
    end if

    if ( this%iteration .eq. chid%unset ) then
      write ( itr_str, "(I0.5)" ) 0
    else
      write ( itr_str, "(I0.5)" ) this%iteration
    end if

    filename = trim(filename) // trim(itr_str) // ".chombo.h5"
  end subroutine rhyme_chombo_filename_generator


  subroutine rhyme_chombo_create_chombo ( this )
    implicit none

    class ( chombo_t ), intent (inout) :: this

    character ( len=1024 ) :: filename

    call this%filename_generator ( filename )
    call this%create ( filename )

    this%is_opened = .true.

  end subroutine rhyme_chombo_create_chombo


  subroutine rhyme_chombo_write_headers ( this, samr )
    implicit none

    class ( chombo_t ), intent (inout) :: this
    type ( samr_t ), intent ( in ) :: samr

    integer :: l
    character ( len=16 ) :: level_name


    if ( .not. this%is_opened ) return


    do l = 0, samr%nlevels - 1
      write ( level_name, '(A7,I1)') "/level_", l
      call this%create_group ( level_name, this%level_ids(l) )
      call this%write_group_1d_array_attr ( level_name, "dx", samr%levels(l)%dx )
      call this%write_group_attr ( level_name, "ref_ratio", samr%levels(l)%refine_factor )
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

    call this%create_group ( "/Chombo_global", this%chombo_global_id )

    call this%write_group_attr ( "/Chombo_global", "SpaceDim", 3 )

  end subroutine rhyme_chombo_write_headers


  subroutine rhyme_chombo_write_level_data ( this, level )
    implicit none

    class ( chombo_t ), intent ( inout ) :: this
    type ( samr_level_t ), intent ( in ) :: level

    integer :: b, var, lb, ub, offset, length, dim1d, dims(3)
    real ( kind=8 ), allocatable :: d(:)
    integer, allocatable :: boxes(:,:)


    if ( .not. this%is_opened ) return


    offset = 1
    length = 0

    do b = 1, level%nboxes
      length = length + product( level%boxes(b)%dims ) * 5
    end do

    allocate ( d( length ) )
    allocate ( boxes( 6, level%nboxes ) )

    do b = 1, level%nboxes
      dims = level%boxes(b)%dims
      dim1d = product( dims )

      boxes( 1:3, b ) = level%boxes(b)%left_edge(:) - 1
      boxes( 4:6, b ) = level%boxes(b)%right_edge(:) - 1

      do var = hyid%rho, hyid%e_tot
        lb = offset + (var - 1) * dim1d
        ub = lb + dim1d - 1

        ! TODO: Instead of reshaping, we can directly copy the array using:
        ! use, intrinsic :: ISO_C_BINDING
        ! real, allocatable, target :: rank1_array(:)
        ! real, pointer :: rank3_array(:,:,:)
        ! call C_F_POINTER (C_LOC(rank1_array), rank3_array, [100,100,1000])
        d ( lb:ub ) = reshape ( &
          level%boxes(b)%hydro(1:dims(1),1:dims(2),1:dims(3))%u(var), &
          [ dim1d ] &
        )
      end do

      offset = offset + 5 * dim1d
    end do

    call this%write_1d_dataset ( this%level_ids(level%level), "data:datatype=0", d )
    call this%write_table ( &
      this%level_ids(level%level), "boxes", &
      [ "lo_i", "lo_j", "lo_k", "hi_i", "hi_j", "hi_k" ], &
      boxes &
    )

    deallocate ( d )
    deallocate ( boxes )
  end subroutine rhyme_chombo_write_level_data


  subroutine rhyme_chombo_write_samr ( this, samr )
    implicit none

    class ( chombo_t ), intent (inout) :: this
    type ( samr_t ), intent (in) :: samr

    integer :: l, hdferr

    this%iteration = samr%levels(0)%iteration
    call this%create_chombo

    call this%write_headers ( samr )

    do l = 0, samr%nlevels - 1
      call this%write_level_data ( samr%levels(l) )
    end do

    ! Closing open groups
    do l = 0, samr%nlevels - 1
      call h5gclose_f ( this%level_ids(l), hdferr )
    end do
    call h5gclose_f ( this%chombo_global_id, hdferr )

    call this%close

    this%iteration = chid%unset
    this%level_ids = chid%unset
    this%chombo_global_id = chid%unset
    this%is_opened = .false.
  end subroutine rhyme_chombo_write_samr
end module rhyme_chombo
