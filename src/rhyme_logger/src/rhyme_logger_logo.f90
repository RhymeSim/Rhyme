submodule(rhyme_logger) logo_smod
contains
module subroutine rhyme_logger_set_colored_logo(this)
   implicit none

   class(logger_t), intent(inout) :: this

   this%colored_logo(1) = tc%nc

   this%colored_logo(2) = tc%rd//"▄"//tc%yl//"▄▄"//tc%gn//"▄▄"//tc%cy//"▄" &
                          //tc%mg//"    ▄"//tc%rd//"▄"//tc%nc

   this%colored_logo(3) = tc%yl//"█"//tc%gn//"█▀"//tc%cy//"▀▀"//tc%bl//"▀█" &
                          //tc%mg//"█"//tc%rd//"  █"//tc%yl//"█"//tc%nc

   this%colored_logo(4) = tc%gn//"█"//tc%cy//"█"//tc%mg//"    █"//tc%rd//"█" &
                          //tc%yl//"  █"//tc%gn//"█▄"//tc%cy//"██"//tc%bl &
                          //"██"//tc%mg//"▄"//tc%rd//"  ▀"//tc%yl//"██" &
                          //tc%cy//"  ██"//tc%bl//"█"//tc%mg//"  █"//tc%rd &
                          //"██"//tc%yl//"█▄"//tc%gn//"██"//tc%cy//"▄" &
                          //tc%mg//"   ▄█"//tc%rd//"██"//tc%yl//"█▄"//tc%nc

   this%colored_logo(5) = tc%cy//"█"//tc%bl//"██"//tc%mg//"██"//tc%rd//"██" &
                          //tc%gn//"   █"//tc%cy//"█▀"//tc%mg//"   █"//tc%rd &
                          //"█"//tc%gn//"   ██"//tc%cy//"▄"//tc%bl//" ██" &
                          //tc%rd//"   █"//tc%yl//"█"//tc%gn//" ██"//tc%cy &
                          //" █"//tc%bl//"█"//tc%mg//"  █"//tc%rd//"█▄" &
                          //tc%yl//"▄▄"//tc%gn//"▄█"//tc%cy//"█"//tc%nc

   this%colored_logo(6) = tc%bl//"█"//tc%mg//"█"//tc%rd//"  ▀"//tc%yl//"██" &
                          //tc%gn//"▄"//tc%cy//"  █"//tc%bl//"█"//tc%rd &
                          //"    █"//tc%yl//"█"//tc%cy//"    █"//tc%bl//"██" &
                          //tc%mg//"█▀"//tc%yl//"   █"//tc%gn//"█"//tc%cy &
                          //" ██"//tc%bl//" █"//tc%mg//"█"//tc%rd//"  █" &
                          //tc%yl//"█▀"//tc%gn//"▀▀"//tc%cy//"▀▀"//tc%bl &
                          //"▀"//tc%nc

   this%colored_logo(7) = tc%mg//"█"//tc%rd//"█"//tc%gn//"    █"//tc%cy &
                          //"█"//tc%bl//"  █"//tc%mg//"█"//tc%yl//"    █" &
                          //tc%gn//"█"//tc%mg//"     ██"//tc%rd//"█"//tc%gn &
                          //"    █"//tc%cy//"█"//tc%bl//" ██"//tc%mg//" █" &
                          //tc%rd//"█"//tc%yl//"  ▀"//tc%gn//"██"//tc%cy &
                          //"▄▄"//tc%bl//"▄▄"//tc%mg//"█"//tc%nc

   this%colored_logo(8) = tc%rd//"▀"//tc%yl//"▀"//tc%cy//"    ▀"//tc%bl &
                          //"▀▀"//tc%mg//" ▀"//tc%rd//"▀"//tc%gn//"    ▀" &
                          //tc%cy//"▀"//tc%rd//"     ██"//tc%cy//"     ▀" &
                          //tc%bl//"▀"//tc%mg//" ▀▀"//tc%rd//" ▀"//tc%yl &
                          //"▀"//tc%cy//"    ▀"//tc%bl//"▀▀"//tc%mg//"▀▀" &
                          //tc%nc

   this%colored_logo(9) = tc%rd//"                      ██"//tc%yl//"█" &
                          //tc%nc//"  © Saeed Sarpas, 2019"//tc%nc

   this%colored_logo(10) = tc%nc
end subroutine rhyme_logger_set_colored_logo

module subroutine rhyme_logger_set_logo(this)
   implicit none

   class(logger_t), intent(inout) :: this

   this%logo(1) = ""
   this%logo(2) = "▄▄▄▄▄▄    ▄▄"
   this%logo(3) = "██▀▀▀▀██  ██"
   this%logo(4) = "██    ██  ██▄████▄  ▀██  ███  ████▄██▄   ▄████▄"
   this%logo(5) = "███████   ██▀   ██   ██▄ ██   ██ ██ ██  ██▄▄▄▄██"
   this%logo(6) = "██  ▀██▄  ██    ██    ████▀   ██ ██ ██  ██▀▀▀▀▀▀"
   this%logo(7) = "██    ██  ██    ██     ███    ██ ██ ██  ▀██▄▄▄▄█"
   this%logo(8) = "▀▀    ▀▀▀ ▀▀    ▀▀     ██     ▀▀ ▀▀ ▀▀    ▀▀▀▀▀"
   this%logo(9) = "                      ▀▀▀  © Saeed Sarpas, 2019"
   this%logo(10) = ""
end subroutine rhyme_logger_set_logo
end submodule logo_smod
