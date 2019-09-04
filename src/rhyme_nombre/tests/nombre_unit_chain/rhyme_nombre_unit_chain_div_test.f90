logical function rhyme_nombre_unit_chain_div_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2

  tester = .describe. "nombre_unit_chain_div"

  c1 => meter / sec
  call tester%expect( c1%head == meter .toBe. .true. .hint. 'c1 head' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev' )
  call tester%expect( c1%head%next == sec**(-1) .toBe. .true. .hint. 'c1 head%next' )
  call tester%expect( c1%head%next%prev == meter .toBe. .true. .hint. 'c1 head%next%prev' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 head%next%next' )

  call tester%expect( c1%dim == rhyme_nombre_unit_chain_get_dim( c1 ) .toBe. .true. .hint. 'c1 dim')

  c2 => c1 / kel
  call tester%expect( c1%head == meter .toBe. .true. .hint. 'c1 head' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev' )
  call tester%expect( c1%head%next == sec**(-1) .toBe. .true. .hint. 'c1 head%next' )
  call tester%expect( c1%head%next%prev == meter .toBe. .true. .hint. 'c1 head%next%prev' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 head%next%next' )

  call tester%expect( c2%head == meter .toBe. .true. .hint. 'c2 head' )
  call tester%expect( associated( c2%head%prev ) .toBe. .false. .hint. 'c2 head%prev' )
  call tester%expect( c2%head%next == sec**(-1) .toBe. .true. .hint. 'c2 head%next' )
  call tester%expect( c2%head%next%prev == meter .toBe. .true. .hint. 'c2 head%next%prev' )
  call tester%expect( c2%head%next%next == kel**(-1) .toBe. .true. .hint. 'c2 head%next%next' )
  call tester%expect( c2%head%next%next%prev == sec**(-1) .toBe. .true. .hint. 'c2 head%next%next%prev' )
  call tester%expect( associated( c2%head%next%next%next ) .toBe. .false. .hint. 'c2 head%next%next%next' )

  call tester%expect( c2%dim == rhyme_nombre_unit_chain_get_dim( c2 ) .toBe. .true. .hint. 'c2 dim')

  failed = tester%failed()
end function rhyme_nombre_unit_chain_div_test
