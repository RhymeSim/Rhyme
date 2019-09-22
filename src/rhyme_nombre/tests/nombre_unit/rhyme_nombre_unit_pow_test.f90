logical function rhyme_nombre_unit_pow_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: du, dui, dur, dur8
  type ( nombre_unit_t ), pointer :: duc, duci, ducr, ducr8

  tester = .describe. "nombre_unit_pow"

  call rhyme_nombre_derived_unit_init

  du => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='N' )

  dui => du**3
  call tester%expect( dui%prefix == du%prefix .toBe. .true. .hint. 'dui prefix' )
  call tester%expect( dui%symb .toBe. du%symb .hint. 'dui symb' )
  call tester%expect( dui%conv .toBe. du%conv .hint. 'dui conv' )
  call tester%expect( dui%pow .toBe. 3d0 .hint. 'dui pow' )
  call tester%expect( dui%dim == du%dim .toBe. .true. .hint. 'dui dim' )

  dur => du**1.23e4
  call tester%expect( dur%prefix == du%prefix .toBe. .true. .hint. 'dur prefix' )
  call tester%expect( dur%symb .toBe. du%symb .hint. 'dur symb' )
  call tester%expect( dur%conv .toBe. du%conv .hint. 'dur conv' )
  call tester%expect( dur%pow .toBe. 1.23e4 .hint. 'dur pow' )
  call tester%expect( dur%dim == du%dim .toBe. .true. .hint. 'dur dim' )

  dur8 => du**5.67d2
  call tester%expect( dur8%prefix == du%prefix .toBe. .true. .hint. 'dur8 prefix' )
  call tester%expect( dur8%symb .toBe. du%symb .hint. 'dur8 symb' )
  call tester%expect( dur8%conv .toBe. du%conv .hint. 'dur8 conv' )
  call tester%expect( dur8%pow .toBe. 5.67d2 .hint. 'dur8 pow' )
  call tester%expect( dur8%dim == du%dim .toBe. .true. .hint. 'dur8 dim' )

  duc => nom_duc_factory%generate_chain( [ joule, hertz, radian ] )

  duci => duc**4
  call tester%expect( duci%prefix == joule%prefix .toBe. .true. .hint. 'duci joule prefix' )
  call tester%expect( duci%symb .toBe. joule%symb .hint. 'duci joule symb' )
  call tester%expect( duci%conv .toBe. joule%conv .hint. 'duci joule conv' )
  call tester%expect( duci%pow .toBe. 4d0 .hint. 'duci joule pow' )
  call tester%expect( duci%dim == joule%dim .toBe. .true. .hint. 'duci joule dim' )

  call tester%expect( duci%next%prefix == hertz%prefix .toBe. .true. .hint. 'duci hertz prefix' )
  call tester%expect( duci%next%symb .toBe. hertz%symb .hint. 'duci hertz symb' )
  call tester%expect( duci%next%conv .toBe. hertz%conv .hint. 'duci hertz conv' )
  call tester%expect( duci%next%pow .toBe. 4d0 .hint. 'duci hertz pow' )
  call tester%expect( duci%next%dim == hertz%dim .toBe. .true. .hint. 'duci hertz dim' )

  call tester%expect( duci%next%next%prefix == radian%prefix .toBe. .true. .hint. 'duci radian prefix' )
  call tester%expect( duci%next%next%symb .toBe. radian%symb .hint. 'duci radian symb' )
  call tester%expect( duci%next%next%conv .toBe. radian%conv .hint. 'duci radian conv' )
  call tester%expect( duci%next%next%pow .toBe. 4d0 .hint. 'duci radian pow' )
  call tester%expect( duci%next%next%dim == radian%dim .toBe. .true. .hint. 'duci radian dim' )

  call tester%expect( associated( duci%next%next%next ) .toBe. .false. .hint. 'duci null end' )

  ducr => duc**2.34e8
  call tester%expect( ducr%prefix == joule%prefix .toBe. .true. .hint. 'ducr joule prefix' )
  call tester%expect( ducr%symb .toBe. joule%symb .hint. 'ducr joule symb' )
  call tester%expect( ducr%conv .toBe. joule%conv .hint. 'ducr joule conv' )
  call tester%expect( ducr%pow .toBe. 2.34e8 .hint. 'ducr joule pow' )
  call tester%expect( ducr%dim == joule%dim .toBe. .true. .hint. 'ducr joule dim' )

  call tester%expect( ducr%next%prefix == hertz%prefix .toBe. .true. .hint. 'ducr hertz prefix' )
  call tester%expect( ducr%next%symb .toBe. hertz%symb .hint. 'ducr hertz symb' )
  call tester%expect( ducr%next%conv .toBe. hertz%conv .hint. 'ducr hertz conv' )
  call tester%expect( ducr%next%pow .toBe. 2.34e8 .hint. 'ducr hertz pow' )
  call tester%expect( ducr%next%dim == hertz%dim .toBe. .true. .hint. 'ducr hertz dim' )

  call tester%expect( ducr%next%next%prefix == radian%prefix .toBe. .true. .hint. 'ducr radian prefix' )
  call tester%expect( ducr%next%next%symb .toBe. radian%symb .hint. 'ducr radian symb' )
  call tester%expect( ducr%next%next%conv .toBe. radian%conv .hint. 'ducr radian conv' )
  call tester%expect( ducr%next%next%pow .toBe. 2.34e8 .hint. 'ducr radian pow' )
  call tester%expect( ducr%next%next%dim == radian%dim .toBe. .true. .hint. 'ducr radian dim' )

  call tester%expect( associated( ducr%next%next%next ) .toBe. .false. .hint. 'ducr null end' )

  ducr8 => duc**1.24d3
  call tester%expect( ducr8%prefix == joule%prefix .toBe. .true. .hint. 'ducr8 joule prefix' )
  call tester%expect( ducr8%symb .toBe. joule%symb .hint. 'ducr8 joule symb' )
  call tester%expect( ducr8%conv .toBe. joule%conv .hint. 'ducr8 joule conv' )
  call tester%expect( ducr8%pow .toBe. 1.24d3 .hint. 'ducr8 joule pow' )
  call tester%expect( ducr8%dim == joule%dim .toBe. .true. .hint. 'ducr8 joule dim' )

  call tester%expect( ducr8%next%prefix == hertz%prefix .toBe. .true. .hint. 'ducr8 hertz prefix' )
  call tester%expect( ducr8%next%symb .toBe. hertz%symb .hint. 'ducr8 hertz symb' )
  call tester%expect( ducr8%next%conv .toBe. hertz%conv .hint. 'ducr8 hertz conv' )
  call tester%expect( ducr8%next%pow .toBe. 1.24d3 .hint. 'ducr8 hertz pow' )
  call tester%expect( ducr8%next%dim == hertz%dim .toBe. .true. .hint. 'ducr8 hertz dim' )

  call tester%expect( ducr8%next%next%prefix == radian%prefix .toBe. .true. .hint. 'ducr8 radian prefix' )
  call tester%expect( ducr8%next%next%symb .toBe. radian%symb .hint. 'ducr8 radian symb' )
  call tester%expect( ducr8%next%next%conv .toBe. radian%conv .hint. 'ducr8 radian conv' )
  call tester%expect( ducr8%next%next%pow .toBe. 1.24d3 .hint. 'ducr8 radian pow' )
  call tester%expect( ducr8%next%next%dim == radian%dim .toBe. .true. .hint. 'ducr8 radian dim' )

  call tester%expect( associated( ducr8%next%next%next ) .toBe. .false. .hint. 'ducr8 null end' )

  failed = tester%failed()
end function rhyme_nombre_unit_pow_test
