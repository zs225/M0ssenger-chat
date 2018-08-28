open Display
open Lwt

(* a [state] represents the state that the app is currently in *)
type state =
  (* [Default] is the "standby" state of the app for after the user logs in.
    * Simply awaits more user commands and displays accordingly *)
  | Default
  | Messaging (* User currently messaging another user(s) *)

(* an [transition] represents a particular task the user wants to perform *)
type transition =
  | Login
  | Create (* Create an account *)
  | Look (* See a list of users and an unread summer *)
  | Chats (* See a list of chats previews *)
  | Message of (string list) (* Begin/enter a chat with [list of usernames] *)
  | Back (* Leave the current chat and go back to Default app state *)
  | Logout (* Goes back to login/create screen *)
  | Exit (* Completely exits the app *)
  | Nop (* When the user doesn't enter anything meaningful *)
  | Help (* Displays a help menu *)
  | Emojis (* Display Emoji help block *)
  | Clear

(* [app_info] holds information about the app state *)
type app_info = {
    client: Client.client;
    state: state;
    disp_intro: bool;
    curr_chat: string list;
    exit_flag: bool ref;
}

(* [disp_state_intro app] prints the appropriate "introduction" string
 * depending on the current state of the app.  Note that
 * [app] is an [app_info] option since login phase cannot populate one yet *)
let disp_state_intro app  =
  match app with
   | Some app -> begin
      match app.state with
      | Default -> disp_app_menu ()
      | Messaging -> disp_msg_intro (app.client
                                    |> Client.conversation_update_buffer
                                    |> Client.peek_messages_people_in_chat)
   end
   | None -> disp_init ()


(* [convert_single_word_cmd s state] is a single word [transition] resulting
 * from the user input command [s]
 *
 * requires:
 * [s] is a one word command (i.e. not :message x y z*)
let convert_single_word_cmd s =
  match String.lowercase_ascii s with
    | ":l" | ":login" -> Login
    | ":c" | ":create" -> Create
    | ":look" -> Look
    | ":logout" -> Logout
    | ":back" |":b" -> Back
    | ":exit" -> Exit
    | ":help" | ":h" -> Help
    | ":clean" | ":clear" -> Clear
    | ":chats" | ":chat" -> Chats
    | ":emojis" | ":emoji" -> Emojis
    | _ -> Nop

(* [convert_string_to_transition s] is a [transition] resulting
 * from the user entering [s] *)
let convert_string_to_transition s =
(* Cannot convert to lowercase here because that mutates usernames *)
  let word_list = String.split_on_char ' ' (String.trim s) in

  (* Helper function takes a list of usernames and recursively
   * emoji-ifies all of them, returning the list of usernames but
   * with all emojis replaced by their respective unicode *)
  let rec names_to_emojis lst acc =
    match lst with
     | [] -> List.rev acc
     | h::t -> names_to_emojis t ((emoji_replace h "")::acc)
  in

  match word_list with
  | [] -> Nop
  | h::[] -> convert_single_word_cmd h
  | h::t when (String.lowercase_ascii h = ":message") ->
    Message (names_to_emojis t [])
  | _ -> Nop

(* [handle_look client] displays a list of users of the app as well as a
 * notification summary.  This takes a [Client.client] since we need
 * one to communicate with the server to obtain the printed information *)
let handle_look client =
  disp_clients_online (Lwt_main.run (Client.get_clients client));
  disp_notif (Lwt_main.run (Client.get_notification_abbreiviation client))

(* [handle_chats] displays a list of chats.  Requires a [client] for similar
 * reasons to [handle_look] *)
let handle_chats client =
  disp_all_chats (Lwt_main.run (Client.get_notification_table client))

(* [handle_clear app] clears the screen and reprints the introduction
 * text depending on the current state of the app (to remind the user
 * what was going on before they cleared) *)
let handle_clear app  =
  clean_screen ();
  disp_state_intro app

(* [trivial_trans app trans] handles the "easier" transitions that do
 * not alter the app state.
 *
 * Commands handled here only really print things.  There should be
 * no recursive calls to anything here
 *
 * [app] is an [app_info] option because this could also be called in the
 * login/create phase when we don't have an [app_info] populated yet
 *
 * requires: [trans] is a valid transition in the current state
 * of the app *)
let trivial_trans app trans =
  match trans with
  | Login | Create | Back | Logout | Message _ ->
      failwith "impossible: command handled elsewhere"
  | Look -> begin
    match app with
     | Some app -> handle_look (app.client)
     | None -> failwith "no app??"
  end
  | Chats -> begin
    match app with
     | Some app -> handle_chats (app.client)
     | None -> failwith "no app??"
  end
  | Exit -> exit 0
  | Nop -> print_endline "That's not a valid command"
  | Help -> disp_help ()
  | Clear -> handle_clear app
  | Emojis -> disp_emojis ()

(* [stop_updating app] stops the application from pulling from the server.
 * This is so we don't keep getting message updates when we exit the
 * Messaging state *)
let stop_updating app =
    app.exit_flag := true;
    Client.stop_update_and_clean_up app.client

(* [main_app app] runs the main logic of the app.  [app] is an [app_info]
 * is passed between function calls to preserve data
 *
 * requires: user has logged in successfully *)
let rec main_app app =
  match app.state with
  | Default -> begin
    if app.disp_intro then (disp_app_menu ());
    let user_input = Pervasives.read_line () in
    erase_user_input ();
    (* Will be Look, Chats, Message, Logout, Exit, Nop, Help *)
    let transition = convert_string_to_transition user_input in
    match transition with
     | Logout -> confirm_logout app
     | Message lst -> begin
        try (
          let _ = Lwt_main.run (Client.do_command app.client
                                              (Client.Get_all_message lst)) in
            main_app ({app with disp_intro = true;
                                state = Messaging;
                                curr_chat = lst})
        )
        with
        (* [Client.do_command] will throw exception if user
         * entered bad usernames *)
         | _ ->
          (disp_msg_init_error (); main_app ({app with disp_intro = false}))
      end
     | Login | Create | Back ->
        disp_bad_cmd (); main_app ({app with disp_intro = false})
     (* Look, Exit, Nop, Help *)
     | t ->
        trivial_trans (Some app) t; main_app ({app with disp_intro = false})
  end
  | Messaging -> begin
    if app.disp_intro
    then disp_msg_intro (app.client
                        |> Client.conversation_update_buffer
                        |> Client.peek_messages_people_in_chat);
    (* Start the pulling thread *)
    let update_client = Client.update_with_freq_f app.client 1. in
    (* Make and run display thread *)
    let disp_thread = app.exit_flag := false;
    Thread.create (fun () -> disp_messages_thread app.client
                                                  3.
                                                  app.exit_flag) () in
    (***********************************************************)
    (* Create an internal messaging loop *)
    let rec msg_loop thread app =
      let user_input = Pervasives.read_line () in
      erase_user_input ();
      (* Allowed: Nop (messaging things), Exit, Help, Logout, Back*)
      let transition = convert_string_to_transition user_input in
      match transition with
      | Nop -> begin
          let emoji_string = emoji_replace user_input "" in
          let _ = Lwt_main.run (Client.do_command app.client
                    (Client.Send_message (app.curr_chat, emoji_string))) in
            msg_loop thread app
        end
      | Logout -> confirm_logout app
      | Back -> ignore(stop_updating app); main_app ({app with state = Default;
                                                        disp_intro = true;
                                                        curr_chat = []})
      | Login | Create | Look | Chats | Message _ ->
          disp_bad_cmd ();
          msg_loop thread app
      | t ->
          trivial_trans (Some app) t; msg_loop thread app
    (***************************************************************)
    in msg_loop disp_thread {app with client = update_client}
  end

(* Does messaging display things *)
and disp_messages_thread client freq exit =
  let current_conversation_buffer = Client.conversation_update_buffer client in
  let rec loop exit =
    if (not !exit) then
    (ignore(Thread.create (fun () -> current_conversation_buffer
                             |> Client.messages_diff_to_display
                             |> List.iter print_endline) ());
    Thread.delay (1. /. freq);
    loop exit) in
  loop exit


(* [confirm_logout app] allows a user to confirm their logout and transitions
 * app to the appropriate state
 *
 * requires:
 * Called after a logout command *)
and confirm_logout app =
  print_string "Do you actually want to log out? (Y/N)\n>";
  let response = Pervasives.read_line () in
  erase_user_input ();
    match String.lowercase_ascii response with
     | "yes" | "y" -> ignore(stop_updating app); init_phase true
     | "no" | "n" -> main_app ({app with disp_intro = false})
     | _ ->
      print_endline "Please answer \"yes\" or \"no\""; confirm_logout app


(****************************START: INIT PHASE *****************************)

(* [init_phase] handles the login/account creation phase of the app.  This
 * is the first bit of app logic that is run when the user first starts up
 * the app.
 *
 * This will keep looping until the user gets a login correct.  Then the
 * program jumps to the main app logic
 *
 * [display_init] is just a flag that determines whether or not to print
 * the introductory string on the screen *)
and init_phase display_init =
  if display_init then disp_init ();
  let user_input = Pervasives.read_line () in
  erase_user_input ();
  (* Valid transition will either be Login, Create, Exit, Help, or Nop *)
  let transition = convert_string_to_transition user_input in
  match transition with
   | Login -> begin
      let curr_client = handle_login () in
      let app_info = {
        client = curr_client;
        state = Default;
        disp_intro = true;
        curr_chat = [];
        exit_flag = ref true;
      } in
      (* successful login calls the main app loop *)
        main_app app_info
   end
   | Create -> handle_create ()
   (* Commands that shouldn't work here *)
   | Look | Chats | Logout | Back | Message _ ->
      disp_bad_cmd (); init_phase false
   (* Exit, Help, or Nop *)
   | t -> trivial_trans None t; init_phase false

(* [handle_create] prompts user to enter info to create an account.
 * Then goes back to init state.  Upon failure, prints a failure message
 *
 * requires:
 * function must be called as a result of a Create transition *)
and handle_create () =
  disp_account_creation ();
  print_string "Please input a username\n>";
  let username = Pervasives.read_line () in
  print_newline();
  (* The user has an opportunity to enter a command on the username
   * field if they want to bail on account creation *)
  let transition = convert_string_to_transition username in
    (* Back, Help, Exit should be the only command that is allowed *)
      match transition with
      | Back -> init_phase true
      | Login | Create | Look | Message _ | Logout | Clear ->
          disp_bad_cmd (); handle_create ()
      | Nop -> begin
        if (String.length (String.trim username) = 0)
        then (disp_invalid_username (); init_phase false)
        else begin
        (* User didn't enter a command so we do regular login procedure *)
          print_string "Please input a password\n>";
          let password = Pervasives.read_line () in
          print_newline();

          let emoji_name = emoji_replace (String.trim username) "" in

          if Lwt_main.run (Client.sign_up emoji_name
                                          password)

          then (disp_create_success (); init_phase true)
          else (disp_create_fail (); init_phase false)
      end
     end
     | t -> trivial_trans None t; handle_create ()

(* [handle_login] prompts user for username/pass and returns a client record
 * if validated or goes back to Init state if failed *)
and handle_login () =
  try
    print_string "Please input your username\n>";
    let username = Pervasives.read_line () in
    print_newline ();
    print_string "Please input your password\n>";
    let password = Pervasives.read_line () in
    print_newline ();

    let emoji_name = emoji_replace (String.trim username) "" in

    let client = Lwt_main.run (Client.log_in emoji_name password
                              >|= fun client -> client) in
    disp_login_success (Client.get_info User_name client);
    client
  with
  | _ -> disp_login_fail (); init_phase false

(****************************END: INIT PHASE *****************************)

(* [start_app] runs the app *)
let start_app () = ignore(init_phase true)
