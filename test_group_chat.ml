open Client
open Lwt

let show_counter = false

(******************************************************************************
 *
 * Test of group chat: set up and pulling
 *
 *****************************************************************************)


let () =  Lwt_main.run (sign_up "test1" "a"
                        >|= string_of_bool
                        >|= print_endline )

let client1 =  Lwt_main.run (log_in "test1" "a")

let () = client1 |> get_info Session_id |> print_endline

let () =  Lwt_main.run (sign_up "test2" "b"
                        >|= string_of_bool
                        >|= print_endline )

let client2 =  Lwt_main.run (log_in "test2" "b")

let () = client2 |> get_info Session_id |> print_endline

let () =  Lwt_main.run (sign_up "test3" "b"
                        >|= string_of_bool
                        >|= print_endline )

let client3 =  Lwt_main.run (log_in "test3" "b")

let () = client3 |> get_info Session_id |> print_endline

(*************************************)

let () = Lwt_main.run ( Client.get_clients client3) |> List.iter print_endline

let client1 =
  Lwt_main.run (do_command client1
              (Send_message (["test2"; "test3"], "start group chat")))

let clinet1 =
  Lwt_main.run (do_command client1
                          (Get_all_message ["test2"; "test3"]))

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


let display_main_thread = Thread.create (fun () -> test_display client1 10.)

let fetching_main = print_endline "test1 start pulling thread";
                    update_with_freq_f client1 2.

let display_main_thread_handler = print_endline "test1 start displaying";
                                  display_main_thread ()


let rec main f = if f then main f

let () = main true

