open Client
open Lwt

let show_counter = false

(******************************************************************************
 *
 * Test of group chat: client 1
 *
 *****************************************************************************)

let client =  Lwt_main.run (log_in "test1" "a")


let client =
  Lwt_main.run (do_command client
                          (Send_message (["test2"; "test3"], "by test1, test No."^Sys.argv.(1))))
