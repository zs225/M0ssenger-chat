open Client
open Lwt

(******************************************************************************
 *
 * Test of sync: sending side (assume the acount is already created)
 *
 *****************************************************************************)


let client1 =  Lwt_main.run (log_in "test1" "a")

let () = client1 |> get_info Session_id |> print_endline

let client1 =  Lwt_main.run (do_command client1 (Send_message (["test2"], "test No."^Sys.argv.(1))))