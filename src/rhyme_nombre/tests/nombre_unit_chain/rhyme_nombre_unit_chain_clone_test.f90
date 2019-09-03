logical function rhyme_nombre_unit_chain_clone_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: chain, clone

  tester = .describe. "nombre_unit_chain_clone"

  chain => kg * meter
  chain%prefix = kilo
  chain%symb = 'chain'
  chain%conv = 1.23d0
  chain%dim = dimid%mass
  chain%pow = 2.34d0

  clone => rhyme_nombre_unit_chain_clone( chain )

  call tester%expect( clone%prefix == kilo .toBe. .true. .hint. 'prefix' )
  call tester%expect( clone%symb .toBe. 'chain' .hint. 'symbol' )
  call tester%expect( clone%conv .toBe. 1.23d0 .hint. 'conversion factor' )
  call tester%expect( clone%dim == dimid%mass .toBe. .true. .hint. 'dimension' )
  call tester%expect( clone%pow .toBe. 2.34d0 .hint. 'power' )

  call tester%expect( clone%head == kg .toBe. .true. .hint. 'kg' )
  call tester%expect( associated( clone%head%prev ) .toBe. .false. .hint. 'kg prev' )

  call tester%expect( clone%head%next == meter .toBe. .true. .hint. 'meter' )
  call tester%expect( clone%head%next%prev == kg .toBe. .true. .hint. 'meter prev' )
  call tester%expect( associated( clone%head%next%next ) .toBe. .false. .hint. 'meter next' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_clone_test
