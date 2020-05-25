submodule(rhyme_logger) logo_smod
contains
module subroutine rhyme_logger_set_colored_logo(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logger%colored_logo(1) = tc%nc

   logger%colored_logo(2) = tc%rd//"▄"//tc%yl//"▄▄"//tc%gn//"▄▄"//tc%cy//"▄" &
                            //tc%mg//"    ▄"//tc%rd//"▄"//tc%nc

   logger%colored_logo(3) = tc%yl//"█"//tc%gn//"█▀"//tc%cy//"▀▀"//tc%bl//"▀█" &
                            //tc%mg//"█"//tc%rd//"  █"//tc%yl//"█"//tc%nc

   logger%colored_logo(4) = tc%gn//"█"//tc%cy//"█"//tc%mg//"    █"//tc%rd//"█" &
                            //tc%yl//"  █"//tc%gn//"█▄"//tc%cy//"██"//tc%bl &
                            //"██"//tc%mg//"▄"//tc%rd//"  ▀"//tc%yl//"██" &
                            //tc%cy//"  ██"//tc%bl//"█"//tc%mg//"  █"//tc%rd &
                            //"██"//tc%yl//"█▄"//tc%gn//"██"//tc%cy//"▄" &
                            //tc%mg//"   ▄█"//tc%rd//"██"//tc%yl//"█▄"//tc%nc

   logger%colored_logo(5) = tc%cy//"█"//tc%bl//"██"//tc%mg//"██"//tc%rd//"██" &
                            //tc%gn//"   █"//tc%cy//"█▀"//tc%mg//"   █"//tc%rd &
                            //"█"//tc%gn//"   ██"//tc%cy//"▄"//tc%bl//" ██" &
                            //tc%rd//"   █"//tc%yl//"█"//tc%gn//" ██"//tc%cy &
                            //" █"//tc%bl//"█"//tc%mg//"  █"//tc%rd//"█▄" &
                            //tc%yl//"▄▄"//tc%gn//"▄█"//tc%cy//"█"//tc%nc

   logger%colored_logo(6) = tc%bl//"█"//tc%mg//"█"//tc%rd//"  ▀"//tc%yl//"██" &
                            //tc%gn//"▄"//tc%cy//"  █"//tc%bl//"█"//tc%rd &
                            //"    █"//tc%yl//"█"//tc%cy//"    █"//tc%bl//"██" &
                            //tc%mg//"█▀"//tc%yl//"   █"//tc%gn//"█"//tc%cy &
                            //" ██"//tc%bl//" █"//tc%mg//"█"//tc%rd//"  █" &
                            //tc%yl//"█▀"//tc%gn//"▀▀"//tc%cy//"▀▀"//tc%bl &
                            //"▀"//tc%nc

   logger%colored_logo(7) = tc%mg//"█"//tc%rd//"█"//tc%gn//"    █"//tc%cy &
                            //"█"//tc%bl//"  █"//tc%mg//"█"//tc%yl//"    █" &
                            //tc%gn//"█"//tc%mg//"     ██"//tc%rd//"█"//tc%gn &
                            //"    █"//tc%cy//"█"//tc%bl//" ██"//tc%mg//" █" &
                            //tc%rd//"█"//tc%yl//"  ▀"//tc%gn//"██"//tc%cy &
                            //"▄▄"//tc%bl//"▄▄"//tc%mg//"█"//tc%nc

   logger%colored_logo(8) = tc%rd//"▀"//tc%yl//"▀"//tc%cy//"    ▀"//tc%bl &
                            //"▀▀"//tc%mg//" ▀"//tc%rd//"▀"//tc%gn//"    ▀" &
                            //tc%cy//"▀"//tc%rd//"     ██"//tc%cy//"     ▀" &
                            //tc%bl//"▀"//tc%mg//" ▀▀"//tc%rd//" ▀"//tc%yl &
                            //"▀"//tc%cy//"    ▀"//tc%bl//"▀▀"//tc%mg//"▀▀" &
                            //tc%nc

   logger%colored_logo(9) = tc%rd//"                      ██"//tc%yl//"█" &
                            //tc%nc//"  © Saeed Sarpas, 2019"//tc%nc

   logger%colored_logo(10) = tc%nc
end subroutine rhyme_logger_set_colored_logo

module subroutine rhyme_logger_set_logo(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logger%logo(1) = ""
   logger%logo(2) = "▄▄▄▄▄▄    ▄▄"
   logger%logo(3) = "██▀▀▀▀██  ██"
   logger%logo(4) = "██    ██  ██▄████▄  ▀██  ███  ████▄██▄   ▄████▄"
   logger%logo(5) = "███████   ██▀   ██   ██▄ ██   ██ ██ ██  ██▄▄▄▄██"
   logger%logo(6) = "██  ▀██▄  ██    ██    ████▀   ██ ██ ██  ██▀▀▀▀▀▀"
   logger%logo(7) = "██    ██  ██    ██     ███    ██ ██ ██  ▀██▄▄▄▄█"
   logger%logo(8) = "▀▀    ▀▀▀ ▀▀    ▀▀     ██     ▀▀ ▀▀ ▀▀    ▀▀▀▀▀"
   logger%logo(9) = "                      ▀▀▀  © Saeed Sarpas, 2019"
   logger%logo(10) = ""
end subroutine rhyme_logger_set_logo
end submodule logo_smod
