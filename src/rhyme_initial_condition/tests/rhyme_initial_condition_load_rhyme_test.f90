logical function rhyme_initial_condition_load_rhyme_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ic_tester

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_rhyme'
  character ( len=1024 ) :: filename

  type ( initial_condition_t ) :: ic_read, ic_write
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch

  integer :: l, b, uid, ub(3)

  ic_tester = .describe. "initial_condition_load_rhyme"

  ic_write = ic_factory%generate( ic_factory%simple_3d, 4 )

  ! Initializing SAMR object
  samr = samr_factory%generate()

  ! Prepare chombo file
  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

  ! Running load_header
  ic_read%type = icid%snapshot
  ic_read%snapshot_type = icid%rhyme
  ic_read%snapshot_path = filename

  samr_read%nlevels = samr%nlevels
  samr_read%base_grid = samr%base_grid
  samr_read%ghost_cells = samr%ghost_cells
  samr_read%max_nboxes = samr%max_nboxes
  samr_read%levels%max_nboxes = samr%max_nboxes

  samr_read%levels%nboxes = 0

  call rhyme_initial_condition_load_rhyme( ic_read, samr_read, ic_factory%logger )

  call ic_tester%expect( (samr_read%levels%nboxes) .toBe. (samr%levels%nboxes) )

  do l = 0, samr_read%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      call ic_tester%expect( samr_read%levels(l)%boxes(b)%dims .toBe. samr%levels(l)%boxes(b)%dims )
      call ic_tester%expect( samr_read%levels(l)%boxes(b)%left_edge .toBe. samr%levels(l)%boxes(b)%left_edge )
      call ic_tester%expect( samr_read%levels(l)%boxes(b)%right_edge .toBe. samr%levels(l)%boxes(b)%right_edge )

      ub = samr%levels(l)%boxes(b)%dims

      do uid = hyid%rho, hyid%e_tot
        call ic_tester%expect( &
          (samr_read%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(uid)) &
          .toBe. (samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(uid)) )
      end do
    end do
  end do

  failed = ic_tester%failed()
end function rhyme_initial_condition_load_rhyme_test
