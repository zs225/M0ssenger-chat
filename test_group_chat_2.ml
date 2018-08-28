open Client
open Lwt

let show_counter = false

(******************************************************************************
 *
 * Test of group chat: client 2
 *
 *****************************************************************************)

let client =  Lwt_main.run (log_in "test2" "b")

let client =  Lwt_main.run (do_command client (Send_message (["test1"; "test3"], "by test2, test No."^Sys.argv.(1))))