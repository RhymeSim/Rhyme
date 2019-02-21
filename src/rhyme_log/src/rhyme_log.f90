module rhyme_log
  use rhyme_logger_util

  implicit none

  type, extends ( logger_util_const_t ) :: log_const_t
  end type log_const_t

  type ( log_const_t ), parameter :: logid = log_const_t ()


  type, extends ( logger_util_t ) :: log_t
  end type log_t

end module rhyme_log
