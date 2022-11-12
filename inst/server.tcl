#!/usr/bin/tclsh

# make sure globals exist:
set line "test"
set sock ""
set server ""
set port 0

proc readConnection {} {
   global line
   global sock
   if {[eof $sock] == 1 || [catch {set line [read $sock]}]} {
      fileevent $sock readable {}
      close $sock
      set sock ""
   } else {
      handleLine
   }
}

proc serverConnection {newSock clientaddr clientport} {

   global sock

   if [string compare $sock ""] {
      # already a connection present -> ignore
      close $newSock
   } else {
      set sock $newSock

      fconfigure $sock -buffering none -blocking 0
      
      fileevent $sock readable readConnection

      puts $sock [clock format [clock seconds]]
   }
}

proc startServer {} {
   global server
   global port
   set server [socket -server serverConnection $port]
}

proc stopServer {} {
   global sock
   global server
   catch {close $sock}
   catch {close $server}
   set sock ""
   set server ""
}


# Defaults (overwrite in R!):
proc handleLine {} {
   global line
   puts "Not handled:"
   puts $line
}

set port 18721

