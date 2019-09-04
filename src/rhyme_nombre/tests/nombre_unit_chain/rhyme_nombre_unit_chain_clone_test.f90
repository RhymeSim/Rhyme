logical function rhyme_nombre_unit_chain_clone_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2, c3, clone

  tester = .describe. "nombre_unit_chain_clone"

  c1 => kg * meter
  c1%prefix = kilo
  c1%symb = 'c1'
  c1%conv = 1.23d0
  c1%dim = dimid%mass
  c1%pow = 2.34d0

  clone => rhyme_nombre_unit_chain_clone( c1 )

  call tester%expect( clone%prefix == kilo .toBe. .true. .hint. 'prefix' )
  call tester%expect( clone%symb .toBe. 'c1' .hint. 'symbol' )
  call tester%expect( clone%conv .toBe. 1.23d0 .hint. 'conversion factor' )
  call tester%expect( clone%dim == dimid%mass .toBe. .true. .hint. 'dimension' )
  call tester%expect( clone%pow .toBe. 2.34d0 .hint. 'power' )

  call tester%expect( clone%head == kg .toBe. .true. .hint. 'kg' )
  call tester%expect( associated( clone%head%prev ) .toBe. .false. .hint. 'kg prev' )

  call tester%expect( clone%head%next == meter .toBe. .true. .hint. 'meter' )
  call tester%expect( clone%head%next%prev == kg .toBe. .true. .hint. 'meter prev' )
  call tester%expect( associated( clone%head%next%next ) .toBe. .false. .hint. 'meter next' )

  c2 => 1 / sec .as. 'Hz'
  c3 => kg * meter**2 / sec**2 .as. 'J'
  c1%next => c2
  c2%next => c3
  c3%prev => c2
  c2%prev => c1

  clone => rhyme_nombre_unit_chain_clone( c1 )
  call tester%expect( clone == c1 .toBe. .true. )
  call tester%expect( clone%next == c2 .toBe. .true. )
  call tester%expect( clone%next%next == c3 .toBe. .true. )
  call tester%expect( clone%next%next%prev == c2 .toBe. .true. )
  call tester%expect( clone%next%next%prev%prev == c1 .toBe. .true. )

  call tester%expect( associated( clone%prev ) .toBe. .false. )
  call tester%expect( associated( clone%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_clone_test
