open StandardTypeProtocol
open Display

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic
open Thread

let debug_lock = false
let show_counter = true
let base_uri = "http://0.0.0.0:8000"

 (* messages bufferred in client from version a to version b
  * the first int is supposed to be display version, and second int is server version
  * initial: -1, -1
  *)
type conversation_update_buffer = ((string list) * int * int)

type readlock = (conversation_update_buffer Lwt_condition.t)

type writelock = Lwt_mutex.t

type conversation_diff = conversation_update_buffer ref * readlock * writelock

(* A [client] stores information needed in a networking communication:
 * e.g. session, type of request, parameters to request etc *)
type client = {
   user_name: string;
   session_id: int;
   current_conversation: conversation_update_buffer ref;
   current_converse_with: string;
   readlock: readlock;
   writelock: writelock;
   update_thread: Thread.t option
}



type command = Send_message of string * string (* another user  message*)
              | Get_all_message of string list
              (* set the message buffer to from version n to current *)
              | Get_messages_diff_since of (string list) * int
              (* | Start_new_conversation | Close_coversation *)
              (* | Get_last_n_messages of (string list)*int *)


(******************************************************************************
 *
 * Accessing information in client
 *
 *****************************************************************************)


(* [current_conversation c] is the reference to the conversation diff list *)
let current_conversation client =
  (client.current_conversation, client.readlock, client.writelock)

(* take message bufferred and clean buffer *)
let messages_diff_to_display conversation_diff =
  let (diff, readlock, writelock) = conversation_diff in
  Lwt_mutex.lock writelock >|= fun () ->
  if debug_lock then print_endline "get read update lock";
  let (messages , start_ , end_) = !diff in
  diff := ([], end_,end_);
  Lwt_mutex.unlock writelock;
  if debug_lock then print_endline "release read update lock";
  messages

(* peek message bufferred but not clean buffer *)
let peek_messages_diff conversation_diff =
  let (diff, readlock, writelock) = conversation_diff in
  (Lwt_condition.wait readlock) >|= fun (messages, _, _) -> messages

(* peek version range bufferred but not clean buffer *)
let peek_messages_diff_version conversation_diff =
  let (diff, readlock, writelock) = conversation_diff in
  (Lwt_condition.wait readlock) >|= fun (_, start_, end_) -> (start_, end_)

(* [get_info client info_kind] retrieves the information of kind [info_kind]from
 * client *)
let get_info client info_kind = failwith "not implemented"

(******************************************************************************
 *
 * Operations that can be done without granted a client
 *
 *****************************************************************************)


let from_login user_name password =
  `Assoc([("username", `String(user_name));
          ("password", `String(password))])
  |> to_string


let log_in username password =
  Client.post
      ~body:(Cohttp_lwt.Body.of_string (from_login username password))
      (Uri.of_string (base_uri^"/login"))
  >>= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then
     body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let id = to_int (member "session_id" (from_string body)) in
        {
          session_id = id;
          user_name = username;
          current_conversation = ref ([] , -1, -1);
          current_converse_with = "";
          readlock = Lwt_condition.create ();
          writelock = Lwt_mutex.create ();
          update_thread = None
        }
  else failwith "invalid login"

let from_signup user_name password connectionpswd realname =
  `Assoc([("username", `String(user_name));
          ("password", `String(password));
          ("connectionpswd", `String(connectionpswd));
          ("realname", `String(realname)) ])
  |> Yojson.Basic.to_string


let sign_up user_name password connectionpswd realname =
  Client.post
      ~body:(Cohttp_lwt.Body.of_string (from_signup user_name password connectionpswd realname))
      (Uri.of_string (base_uri^"/create-account"))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Created then true else false

(* [get_clients] returns a list of client usernames *)
let get_clients () =
   Client.get (Uri.of_string (base_uri^"/see-clients"))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  body |> Cohttp_lwt.Body.to_string >|= fun body -> transform_json_clients body

(******************************************************************************
 *
 * Operations that must be done with a client
 *
 *****************************************************************************)

let from_send_messgae username realname targetuser msg =
  `Assoc([("username", `String(username));
          ("realname", `String(realname));
          ("targetuser", `String(targetuser));
          ("msg", `String(msg)) ])
  |> Yojson.Basic.to_string

let send_message user message client =
  Client.post
       ~body:(Cohttp_lwt.Body.of_string (from_send_messgae client.user_name "???" user message))
       (Uri.of_string (base_uri^"/chat?session_id="^string_of_int client.session_id))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then true else false


(* just one people, update the buffer to message in  from version to newest version innserver *)
let update_conversation_to_since (one::[]) version client =
    Client.get
        (* start from [version], should respond [newest_version] *)
        (Uri.of_string (base_uri^"/getmsg?session_id="^(string_of_int client.session_id)^"&username="^(client.user_name)^"&msgs_from="^one))
    >>= fun (resp, body) ->
    let status = resp |> Response.status in
    if status = `Accepted then
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
         let messages = transform_json_messages body in
         (* todo *)
         let newest_version = List.length messages - 1 in
         let rec drop n h =
            if n == 0 || h = [] then h else (drop (n-1) (match h with a::b -> b | [] -> [])) in
         let diff_messages = if version = -1 then messages else drop (version + 1) messages in
         (* todo: currently just do the diff in client side *)
         client.current_conversation := (diff_messages, version, newest_version);
         { client with
           current_converse_with = one
         }
    else (print_endline "err"; return client)

let do_command client command =
  match command with
  | Send_message (user, message) -> send_message user message client; return client
  | Get_all_message group -> update_conversation_to_since group  0 client
  | Get_messages_diff_since (group, version) -> update_conversation_to_since group version client


(******************************************************************************
 *
 * Pulls
 *
 *****************************************************************************)

(* [update c] updates the conversation buffer with information from the server *)
let update client =
  let id = Thread.self () |> Thread.id |> string_of_int in
  if debug_lock then print_endline (id^" want to get update lock");
  let writelock = client.writelock in
  Lwt_mutex.lock writelock >|= fun () ->
    if debug_lock then print_endline (id^" get update lock");
    let (messages , start_ , end_) = !(client.current_conversation) in
    if debug_lock then print_endline (id^" read content");
    (Lwt.catch  (fun () -> Lwt_unix.with_timeout 1.0 (fun () ->
    update_conversation_to_since [client.current_converse_with] start_ client)) (fun _ -> print_endline "time out";return client)) >|= fun client ->
    if debug_lock then print_endline (id^" to check lock");
    if Lwt_mutex.is_empty writelock
    then (if debug_lock then (print_endline (id^" checked lock"));Lwt_condition.broadcast client.readlock !(client.current_conversation));
    Lwt_mutex.unlock writelock;
    (if debug_lock then print_endline (id^" release update lock"));
    client


let rec update_with_freq_f client f =
   let pulling_thread = Thread.create (fun () ->
     let counter = ref 0 in
     let rec loop counter =
        Thread.create (fun () -> Lwt_unix.run (update client)) ();
        counter := !counter + 1;
        if show_counter then print_endline ("#"^(string_of_int !counter));
        Thread.delay (1. /. f);
        loop counter in
     Lwt_main.run(loop counter)) in
   match  client.update_thread with
   | Some t -> Thread.kill t; {client with update_thread = Some (pulling_thread ()) }
   | None -> {client with update_thread = Some (pulling_thread ()) }

let rec stop_update_and_clean_up client =
   match  client.update_thread with
   | Some t -> Thread.kill t; { client with
                                current_converse_with = "";
                                current_conversation = ref ([] , -1, -1)
                              }
   | None -> client


(******************************************************************************
 *
 * Tests
 *
 *****************************************************************************)

(*
let () =  Lwt_main.run (sign_up "test1" "a" "c" "test1" >|= string_of_bool >|= print_endline )
let client1 =  Lwt_main.run (log_in "test1" "a" >|= fun client -> client.session_id |> string_of_int |> print_endline; client)

let () =  Lwt_main.run (sign_up "test2" "b" "c" "test2" >|= string_of_bool >|= print_endline )
let client2 =  Lwt_main.run (log_in "test2" "b" >|= fun client -> client.session_id |> string_of_int |> print_endline; client)


let client1 =  Lwt_main.run (do_command client1 (Send_message ("test2", "just a test")))

let clinet2 =  Lwt_main.run (do_command client2 (Get_all_message ["test1"]))

let () = print_endline "one pull"

let () = Lwt_main.run(clinet2 |> current_conversation |> messages_diff_to_display >|= List.iter print_endline)

let rec test_display client f =
   let current_conversation_buffer = current_conversation client in
   let flag = ref true in
   let rec loop flag =
      Thread.create (fun () -> Lwt_main.run (messages_diff_to_display current_conversation_buffer >|= List.iter print_endline)) ();
      Thread.delay (1. /. f);
      loop flag in
    Lwt_main.run(loop flag)


let clinet2 = print_endline "start pulling thread"; update_with_freq_f clinet2 10.0

let () = print_endline "start displaying"

let display_main = test_display client2 60.0


let json_test =
  let username = `String("test2") in
  let password = `String("test1pswd") in
  let connectionpswd = `String("test1connectionpswd") in
  let realname = `String("test1realname") in
  `Assoc([("username", username);
          ("realname", realname);
          ("password",password);
          ("connectionpswd", connectionpswd);
          ("targetuser", `String "test1");
          ("msg", `String "FROM 2 TO 1 OW HOW ");
          ("msgs_from", `String "test1")])
  |> Yojson.Basic.to_string

let body =
  Client.post ~body:(Cohttp_lwt.Body.of_string json_test) (Uri.of_string "http://0.0.0.0:8000/getmsg?session_id=132350") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body *)