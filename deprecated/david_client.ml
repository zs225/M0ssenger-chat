open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

let client_list = ref []

let username_global = ref ""

let msg_lst_global = ref []

let session_id = ref 0

let uri_string = ref ""

let fodder () =
  Lwt.bind (Lwt_unix.sleep 0.5)
    (fun () -> (Lwt_io.print ("")))

let cur_looper = ref (fodder ())

(* [create_account ()] tells the server to create an account*)
let rec create_account () =
  try
    print_string "Please input a username\n>";
    let username = Pervasives.read_line () in
    username_global := username;
    print_newline();
    print_string "Please input a password\n>";
    let password = Pervasives.read_line () in
    print_newline();
    print_string "Please input your real name\n>";
    let realname = Pervasives.read_line () in
    print_newline();
    print_string "Please input a connection password\n>";
    let connectionpswd = Pervasives.read_line () in
    print_newline();
    let json_str =
    `Assoc([("username", `String username);
          ("realname", `String realname);
          ("password", `String password);
          ("connectionpswd", `String connectionpswd);])
    |> Yojson.Basic.to_string in
    let output = Client.post ~body:(Cohttp_lwt.Body.of_string json_str)
    (Uri.of_string ((!uri_string) ^ "create-account"))
    >>= fun (_, s_ret) ->
    s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
    let real_body = Lwt_main.run output in
    if real_body = "N" then
      begin
        print_string "Account creation unsuccessful. Try again.";
        print_newline (); create_account ()
      end
    else
      begin
        print_string "Account creation successful; please login." ;
        print_newline (); login ()
      end
  with
  | _ -> print_newline (); print_string "URL Bad, please reinput.\n"; main ()

(* [login ()] tells the server to create an login in current user*)
and login () =
  try
    print_string "Please input your username\n>";
    let username = Pervasives.read_line () in
    username_global := username;
    print_newline ();
    print_string "Please input your password\n>";
    let password = Pervasives.read_line () in
    print_newline ();
    let json_str =
    `Assoc([("username", `String username);
          ("password", `String password)])
    |> Yojson.Basic.to_string in
    let output = Client.post ~body:(Cohttp_lwt.Body.of_string json_str)
    (Uri.of_string ((!uri_string) ^ "login"))
    >>= fun (_, s_ret) ->
    s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
    let real_body = Lwt_main.run output in
    if real_body = "N" then
      begin
        print_string "Login unsuccessful. Try again.";
        print_newline (); login ()
      end
    else
      begin
        let response_json = real_body |> Yojson.Basic.from_string in
        session_id := to_int (member "session_id" response_json);
        print_string "Login successful. You may now start chatting.";
        print_newline (); menu ()
      end
  with
  | _ -> print_newline (); print_string "URL Bad, please reinput.\n"; main ()

(* [menu ()] displays the primary options available to the user*)
and menu () =
  print_string "Welcome to the CamlChat menu.\n";
  print_string "Type start to enter the chat creation session.\n";
  print_string "Type list to see the other clients available.\n";
  print_string "Type exit to leave CamlChat.\n>";
  match Pervasives.read_line () with
  | "start" -> start_chat ()
  | "list" -> list_clients ()
  | "exit" -> ()
  | _ -> menu ()

(* [list_clients ()] tells server to return the list of clients who created
 * an account *)
and list_clients () =
  try
    let output = Client.get
    (Uri.of_string
      (((!uri_string) ^ "see-clients?session_id=")
        ^ (Pervasives.string_of_int (!session_id))))
    >>= fun (_, s_ret) ->
    s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
    let real_body = Lwt_main.run output in
    if real_body = "N" then
      begin
        print_string "Credentials poor. Try again.";
        print_newline (); login ()
      end
    else
      begin
        print_newline ();
        let response_json = real_body |> Yojson.Basic.from_string in
        let lst_of_clients = to_list (member "clients" response_json)
          |> List.map (fun a -> to_string a) in
        List.iter (fun a -> print_string a; print_newline ()) lst_of_clients;
        print_newline (); menu ()
      end
  with
  | _ -> menu ()

(* [start_chat ()] initiates the chat creation process where the user
 * inputs the usernames of other people they wish to speak with*)
and start_chat () =
  print_newline ();
  print_string "Type the usernames of the people you would like to talk to.\n";
  print_string "Type # to start chatting\n>";
  match Pervasives.read_line () with
  | "#" -> start_chat_process ()
  | x -> client_list := x::(!client_list); start_chat ()

(* [start_chat_process ()] displays all the messages sent in the chat session*)
and start_chat_process () =
  print_newline ();
  match (!client_list) with
  | [] -> print_string "Please input at least one other user.\n"; start_chat ()
  | _ ->
    try
      let other_users = List.fold_left (fun acc i -> acc ^ "&msgs_from=" ^ i)
        "" (!client_list) in
      let uri_string_real = (!uri_string) ^ "getmsgs?session_id="
      ^ (Pervasives.string_of_int (!session_id)) ^ "&username="
      ^ (!username_global) ^ other_users in
      let output = Client.get
        (Uri.of_string
        ((uri_string_real)))
        >>= fun (_, s_ret) ->
      s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
      let real_body = Lwt_main.run output in
      if real_body = "N" then
        begin
          print_string "Chat unsuccessful. Try again.";
          print_newline (); client_list := []; start_chat ()
        end
      else
        begin
          let response_json = real_body |> Yojson.Basic.from_string in
          let lst_msgs = to_list (member "msgs" response_json)
          |> List.map (fun a -> to_string a) |> List.rev in
          msg_lst_global := lst_msgs;
          List.iter (fun a -> print_string a; print_newline ())
            (!msg_lst_global);
          let looping = update_looper () in
          cur_looper := looping;
          send_msg ()
        end
    with
    | _ -> client_list := [];
          print_string "\nUsers not found. Please retry.\n"; start_chat ()

(* [send_msg ()] tells the server a msg the client wishes to send *)
and send_msg () =
  match (!client_list) with
  | [] -> print_string "Please input at least one other user.\n"; start_chat ()
  | _ ->
    try
      let msg_p = Lwt_io.(read_line stdin) in
      match (Lwt_main.run (msg_p)) with
      | "#" -> Lwt.cancel (!cur_looper); client_list := []; menu ()
      | x ->
        begin
          let uri_string_real = (!uri_string) ^ "chat?session_id="
            ^ (Pervasives.string_of_int (!session_id)) in
          let other_users = List.map (fun a -> `String a) (!client_list) in
          let json_str =
            `Assoc([("username", `String (!username_global));
            ("targetusers", `List other_users); ("msg", `String x)])
            |> Yojson.Basic.to_string in
          let output =
            Client.post ~body:(Cohttp_lwt.Body.of_string json_str)
            (Uri.of_string (uri_string_real))
              >>= fun (_, s_ret) ->
              s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
          let real_body = Lwt_main.run output in
          if real_body = "N" then
            begin
              print_string "Try again from the menu.";
              print_newline (); menu ()
            end
          else
            begin
              ANSITerminal.move_cursor 0 (-1);
              ANSITerminal.erase (Below);
              send_msg ()
            end
        end
    with
    | _ -> print_string "Users not found. Please retry.\n"; start_chat ()

(* [retrieve_updates ()] is a constant background process that gets
 * updates from the server regardin the current chat session *)
and retrieve_updates () =
    try
      let other_users = List.fold_left (fun acc i -> acc ^ "&msgs_from=" ^ i)
        "" (!client_list) in
      let uri_string_real = (!uri_string) ^ "getmsgs?session_id="
      ^ (Pervasives.string_of_int (!session_id)) ^ "&username="
      ^ (!username_global) ^ other_users in
      let output = Client.get
        (Uri.of_string
        ((uri_string_real)))
      >>= fun (_, s_ret) ->
      s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
      let real_body = Lwt_main.run output in
      if real_body = "N" then
        begin
          ""
        end
      else
        begin
          let response_json = real_body |> Yojson.Basic.from_string in
          let lst_msgs = to_list (member "msgs" response_json)
          |> List.map (fun a -> to_string a) |> List.rev in
          get_real_msg_updates lst_msgs (!msg_lst_global)
        end
    with
    | _ -> ""

(* [get_real_msg_updates n o] is a string that represents what new messages
 * the client has not seen yet *)
and get_real_msg_updates n o =
  if (List.length n) = (List.length o) then
    begin
      ""
    end
  else
    begin
      msg_lst_global := n;
      get_last_few ((List.length n) - (List.length o)) n
      |> List.fold_left (fun acc i -> acc ^ i ^ "\n") ""
    end

(* [get_last_few num lst] gets the last [num] elements of [lst] *)
and get_last_few num lst =
  let count = ref 0 in
  List.fold_left
    (fun acc i -> if (!count < num) then (count := !count + 1; i::acc)
      else acc) [] (List.rev lst)

(* [update_looper ()] is effectively a background process that continuously
 * runs during the chat process to get the newest messages *)
and update_looper () =
  Lwt.bind (Lwt_unix.sleep 0.5)
    (fun () -> ignore (Lwt_io.print (retrieve_updates ()));
    update_looper ())

(* [main ()] is the startoff point for user input; they input a uri
 * and login or account creation *)
and main () =
  print_string "Welcome to CamlChat. Please enter the uri of the server,\n";
  print_string "ie. http://0.0.0.0:8080/ or a similar form.\n";
  uri_string := (Pervasives.read_line ());
  print_string ("Please press C to create an account, L to login,\n" ^
    "B to reinput URL.\n>");
  match Pervasives.read_line () with
  | "C" -> print_newline (); create_account ()
  | "L" -> print_newline (); login ()
  | _ -> print_newline (); main ()

let () = main ()

