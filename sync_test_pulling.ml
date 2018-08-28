open Client
open Lwt

let show_counter = false

(******************************************************************************
 *
 * Test of sync: pulling side
 *
 *****************************************************************************)


let () =  Lwt_main.run (sign_up "test1" "a" >|= string_of_bool >|=print_endline)
let client1 =  Lwt_main.run (log_in "test1" "a")

let () = client1 |> get_info Session_id |> print_endline

let () =  Lwt_main.run (sign_up "test2" "b" >|= string_of_bool >|=print_endline)
let client2 =  Lwt_main.run (log_in "test2" "b")

let () = client2 |> get_info Session_id |> print_endline

let client1 =  Lwt_main.run (do_command client1
                            (Send_message (["test2"], "just a test 1")))

let client2 =  Lwt_main.run (do_command client2 (Get_all_message ["test1"]))

let () = print_endline "one pull"

let () = client2 |> conversation_update_buffer
                  |> messages_diff_to_display |> List.iter print_endline


let rec test_display client f =
   let current_conversation_buffer = conversation_update_buffer client in
   let counter = ref 0 in
   let rec loop counter =
      Thread.create (fun () -> current_conversation_buffer
                              |> messages_diff_to_display
                              |> List.iter print_endline ) ();
      counter := !counter + 1;
      if show_counter then print_endline ("#R "^(string_of_int !counter));
      Thread.delay (1. /. f);
      loop counter in
   loop counter


let display_main_thread = Thread.create (fun () -> test_display client2 10.)

let fetching_main = print_endline "start pulling thread";
                    update_with_freq_f client2 2.

let display_main_thread_handler = print_endline "start displaying";
                                  display_main_thread ()


let rec main f = if f then main f

let () = main true

