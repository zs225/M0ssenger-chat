open Client
open Lwt

let show_counter = false

(******************************************************************************
 *
 * Test of group chat: client 3
 *
 *****************************************************************************)

let client =  Lwt_main.run (log_in "test3" "b")

let client =  Lwt_main.run (do_command client (Send_message (["test2"; "test1"], "by test3, test No."^Sys.argv.(1))))