module rhyme_nombre_dimension_factory
   use rhyme_nombre_dimension
   use rhyme_assertion

   implicit none

   type rhyme_nombre_dimension_factory_t
      logical :: initialized = .false.
   contains
      procedure :: init => rhyme_nombre_dimension_factory_init
   end type rhyme_nombre_dimension_factory_t

   type(rhyme_nombre_dimension_factory_t) :: nom_dim_factory = rhyme_nombre_dimension_factory_t()

contains

   subroutine rhyme_nombre_dimension_factory_init(this)
      implicit none

      class(rhyme_nombre_dimension_factory_t), intent(inout) :: this

      this%initialized = .true.
   end subroutine rhyme_nombre_dimension_factory_init
end module rhyme_nombre_dimension_factory
