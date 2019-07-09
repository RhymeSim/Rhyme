module rhyme_logger
  use rhyme_logger_util

  implicit none

  type, extends ( logger_util_const_t ) :: logger_const_t
  end type logger_const_t

  type ( logger_const_t ), parameter :: logid = logger_const_t ()


  type, extends ( logger_util_t ) :: logger_t
  end type logger_t

end module rhyme_logger
