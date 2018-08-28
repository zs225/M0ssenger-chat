open StandardTypeProtocol

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util


(* [debug_lock] set the debug synchronization mode *)
let debug_lock = false


(* [show_counter] show the counters for sending pulling threads *)
let show_counter = false


(* [base_uri] is the uri that client send request to *)
let base_uri = "http://0.0.0.0:8080"


(* [conversation_diff] records conversation pulled from server but haven't been
 * displayed. Entries: (group, msgs, start, end)
 * [group] : people in conversation
 * [msgs]: messages bufferred in client, from version [start] to version [end]
 * [start]: display version, initial value: -1
 * [end]: server version, initial value: -1
 *)
type conversation_diff = ((string list) * (string list) * int * int)


type conversation_update_buffer = conversation_diff ref * Lock.t


type client = {
   user_name: string;
   session_id: int;
   current_conversation: conversation_diff ref;
   lock: Lock.t;
   pulling_flag: ( bool ref ) option
}


type command = Send_message of string list * string
              | Get_all_message of string list
              | Get_messages_diff_since of (string list) * int

type info_kind = User_name | Session_id

(******************************************************************************
 *
 * Accessing information in client
 *
 *****************************************************************************)


let conversation_update_buffer client =
  (client.current_conversation, client.lock)


let messages_diff_to_display conversation_update_buffer =
  let id = Thread.self () |> Thread.id |> string_of_int in
  let (diff, lock) = conversation_update_buffer in
  if debug_lock then print_endline (id^"want to get read update lock");
  Lock.enter_write lock;
  if debug_lock then print_endline (id^"get read update lock");
  let (group, messages , start_ , end_) = !diff in
  diff := (group, [], end_,end_);
  Lock.leave_write lock;
  if debug_lock then print_endline (id^"release read update lock");
  messages


let peek_messages_diff conversation_update_buffer =
  let id = Thread.self () |> Thread.id |> string_of_int in
  let (diff, lock) = conversation_update_buffer in
  if debug_lock then print_endline (id^" want to peek messages");
  Lock.enter_read lock;
  if debug_lock then print_endline (id^" peek messages");
  let (_, messages , _ , _) = !diff in
  Lock.leave_read lock;
  if debug_lock then print_endline (id^" leave peek");
  messages


(* [peek_messages_diff_version ]peek version range bufferred but not clean
 * buffer *)
let peek_messages_diff_version conversation_update_buffer =
  let id = Thread.self () |> Thread.id |> string_of_int in
  let (diff, lock) = conversation_update_buffer in
  if debug_lock then print_endline (id^" peek diff version");
  Lock.enter_read lock;
  let (_, _ , start_ , end_) = !diff in
  Lock.leave_read lock;
  (start_, end_)


let peek_messages_people_in_chat conversation_update_buffer =
  let id = Thread.self () |> Thread.id |> string_of_int in
  let (diff, lock) = conversation_update_buffer in
  if debug_lock then print_endline (id^" peek group");
  Lock.enter_read lock;
  let (group, _ , _ , _) = !diff in
  Lock.leave_read lock;
  group


let get_info info_kind client =
  match info_kind with
  | User_name -> client.user_name
  | Session_id -> string_of_int client.session_id


(******************************************************************************
 *
 * Operations that can be done without granted a client
 *
 *****************************************************************************)


let log_in username password =
  Client.post
      ~body:(Cohttp_lwt.Body.of_string (from_login username password))
      (Uri.of_string (base_uri^"/login"))
  >>= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then
     body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let id = to_session_id body in
        {
          session_id = id;
          user_name = username;
          current_conversation = ref ([], [] , -1, -1);
          lock = Lock.create_lock ();
          pulling_flag = None
        }
  else failwith "invalid login"


let sign_up user_name password =
  Client.post
      ~body:(Cohttp_lwt.Body.of_string (from_signup user_name password))
      (Uri.of_string (base_uri^"/create-account"))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Created then true else false

(******************************************************************************
 *
 * Operations that must be done with a client, and only has side effects
 *
 *****************************************************************************)

let send_message users message client =
  Client.post
       ~body:(Cohttp_lwt.Body.of_string (from_send_message client.user_name
                                                           users message))
       (Uri.of_string (base_uri^"/chat?session_id="^
                      string_of_int client.session_id))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  if status <> `Accepted then failwith "invalid message sending"



(* [update_conversation_to_since_unsafe group version client] update buffer
 * to start from version [version] to latest version in server. Should not be
 * used without sync *)
let update_conversation_to_since_unsafe group version client =
    let from_list =
        List.fold_left (fun acc one -> acc^"&msgs_from="^one ) "" group in
    Client.get
        (* start from [version], should respond [newest_version] *)
        (Uri.of_string (base_uri^
                        "/getmsgs?session_id="^(string_of_int client.session_id)
                        ^"&username="^(client.user_name)^from_list))
    >>= fun (resp, body) ->
    let status = resp |> Response.status in
    if status <> `Accepted then failwith "invalid  chat"
    else body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let messages = to_messages body in
        let newest_version = List.length messages in
        let rec take n acc h =
          if n == 0 || h = [] then acc
          else match h with
              |[] -> acc
              | a::b -> (take (n-1) (a::acc) b) in
        let diff_messages = if version < 0 then messages
                            else take (newest_version - version) [] messages in
        client.current_conversation :=
          (group , diff_messages, version, newest_version)

(* [update_conversation_to_since group version client since_this] if not
 * [since_this], update buffer to start from version [version] to latest version
 * in server. Otherwise, update the buffer to start from current end version to
 * latest version in server *)
let update_conversation_to_since group version client since_this =
  let id = Thread.self () |> Thread.id |> string_of_int in
  let get_update_lock = fun () ->
      if debug_lock then print_endline (id^" want to get update lock");
      Lock.enter_write client.lock in
  let update = fun () ->
      if debug_lock then print_endline (id^" update");
      let (curr_group, _ , start_ , end_) = !(client.current_conversation) in
      if since_this
      then update_conversation_to_since_unsafe curr_group start_ client
      else update_conversation_to_since_unsafe group version client in
  let release_lock = fun () ->
      if debug_lock then print_endline (id^" release update lock");
      Lock.leave_write client.lock in
  get_update_lock ();
  try
    Lwt_main.run(update ());
    release_lock ()
  with e -> release_lock (); raise e


(* [get_clients client] is a string list of all of the users of the app.*)
let get_clients client =
   Client.get (Uri.of_string (base_uri^"/see-clients?session_id="^
                                            (string_of_int client.session_id)))
  >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> to_clients body


(* [do_command] executes [command] on the [client] *)
let do_command client command =
  match command with
  | Send_message (users, message) ->
    send_message users message client >|= fun () -> client
  | Get_all_message group ->
    update_conversation_to_since group 0 client false; return client
  | Get_messages_diff_since (group, version) ->
    update_conversation_to_since group version client false ; return client


(******************************************************************************
 *
 * Pulls
 *
 *****************************************************************************)


let rec update_with_freq_f client f =
   let exit_flag = ref false in
   let pulling_thread = Thread.create (fun () ->
     let counter = ref 0 in
     let rec loop counter exit_flag =
        if (not !exit_flag) then
          (ignore(Thread.create (fun () ->
          update_conversation_to_since [] (-1) client true) ());
          counter := !counter + 1;
          if show_counter then print_endline ("#W "^(string_of_int !counter));
          Thread.delay (1. /. f);
          loop counter exit_flag) in
     loop counter exit_flag) in
   match client.pulling_flag with
   | Some t -> t := true;
              ignore(pulling_thread ());
              {client with pulling_flag = Some exit_flag }
   | None -> ignore(pulling_thread ());
             {client with pulling_flag = Some exit_flag }

let rec stop_update_and_clean_up client =
   match client.pulling_flag with
   | Some t -> t := true; { client with
                                current_conversation = ref ([], [] , -1, -1)
                              }
   | None -> client


(******************************************************************************
 *
 * Operations that must be done with a client, return a information
 *
 *****************************************************************************)

let get_notification_table client =
  Client.get
        (Uri.of_string (base_uri^"/self-status?session_id="^
            (string_of_int client.session_id)^"&username="^(client.user_name)))
    >>= fun (resp, body) ->
    let status = resp |> Response.status in
    if status <> `Accepted then failwith "invalid user"
    else body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body |> to_notif



(* returns an ordered pair of int * string list, with the int representing
 * total number of unread and  *)
let get_notification_abbreiviation client =
  get_notification_table client >|= fun notif ->
      let num = List.fold_left (fun acc (_, num, _, _) -> acc + num) 0 notif in
      let senders =
        List.fold_left (fun acc (group, num, _, _) -> if (num > 0) then
          acc @ group else acc)
        [] notif
        |> List.filter (fun a -> a <> "")
        |> List.sort_uniq Pervasives.compare
        |> List.filter (fun a -> a <> client.user_name) in
      (num, senders)


