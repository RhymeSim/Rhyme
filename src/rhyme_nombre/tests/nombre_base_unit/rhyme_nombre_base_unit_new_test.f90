logical function rhyme_nombre_base_unit_new_test () result ( failed )
  use rhyme_nombre_base_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: bu

  tester = .describe. "nombre_base_unit_new"

  bu => rhyme_nombre_base_unit_new()

  call tester%expect( bu%prefix == null_prefix .toBe. .true. )
  call tester%expect( bu%symb .toBe. '' )
  call tester%expect( bu%dim == dimid%null .toBe. .true. )
  call tester%expect( bu%pow .toBe. 1d0 )
  call tester%expect( associated( bu%next ) .toBe. .false. )
  call tester%expect( associated( bu%prev ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_new_test
