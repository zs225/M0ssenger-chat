open StandardTypeProtocol
open Display

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Yojson.Basic

(* A [client] stores information needed in a networking communication:
 * e.g. session, type of request, parameters to request etc *)
type client = {
   user_name: string;
   session_id: int;
   current_conversation: string list
}

type command = Send_message of string * string (* another user  message*)
              | Get_all_message of string list
              (* | Get_messages_from of (string list)*float *)
              (* | Start_new_conversation | Close_coversation *)
              (* | Get_last_n_messages of (string list)*int *)

(* [id] is a user id *)
type id = string

(* [update c] updates the conversation with information from the server *)
let update client = failwith "not implemented"

(* [update c] updates the conversation with information from the server *)
let current_conversation client =
  failwith "not implemented"

(* [get_info client info_kind] retrieves the information of kind [info_kind]from
 * client *)
let get_info client info_kind = failwith "not implemented"


let from_login user_name password =
  `Assoc([("username", `String(user_name));
          ("password", `String(password))])
  |> to_string


let log_in username password =
  Client.post ~body:(Cohttp_lwt.Body.of_string (from_login username password)) (Uri.of_string "http://0.0.0.0:8000/login")
  >>= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let id = to_int (member "session_id" (from_string body)) in
    { session_id = id; user_name = username; current_conversation = []}
  else failwith "invalid login"

let from_signup user_name password connectionpswd realname =
  `Assoc([("username", `String(user_name));
          ("password", `String(password));
          ("connectionpswd", `String(connectionpswd));
          ("realname", `String(realname)) ])
  |> Yojson.Basic.to_string


let sign_up user_name password connectionpswd realname =
  Client.post ~body:(Cohttp_lwt.Body.of_string (from_signup user_name password connectionpswd realname)) (Uri.of_string "http://0.0.0.0:8000/create-account")
  >|= fun (resp, body) -> 
  let status = resp |> Response.status in
  if status = `Created then true else false

let from_send_messgae username realname targetuser msg =
  `Assoc([("username", `String(username));
          ("realname", `String(realname));
          ("targetuser", `String(targetuser));
          ("msg", `String(msg)) ])
  |> Yojson.Basic.to_string |> fun s -> print_endline s;s

let send_message user message client =
  Client.post ~body:(Cohttp_lwt.Body.of_string (from_send_messgae client.user_name "???" user message)) (Uri.of_string ("http://0.0.0.0:8000/chat?session_id="^string_of_int client.session_id))
  >|= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then true else false

let from_get_all_messgae username msgs_from =
  `Assoc([("username", `String(username));
          ("msgs_from", `String(msgs_from)) ])
  |> Yojson.Basic.to_string


(* just one people *)
let get_all_message (one::[]) client =
  Client.get
      (* start from [version], should respond [newest_version] *)
      (Uri.of_string ("http://0.0.0.0:8000/getmsg?session_id="^(string_of_int client.session_id)^"&username="^(client.user_name)^"&msgs_from="^one))
  >>= fun (resp, body) ->
  let status = resp |> Response.status in
  if status = `Accepted then
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let messages = transform_json_messages body in
    { client with current_conversation = messages}
  else failwith "invalid message sending "

let do_command client command =
  match command with
  | Send_message (user, message) -> send_message user message client; return client
  | Get_all_message group -> get_all_message group client


(*tests*)
let () =  Lwt_main.run (sign_up "test1" "a" "c" "test1" >|= string_of_bool >|= print_endline )
let client1 =  Lwt_main.run (log_in "test1" "a" >|= fun client -> client.session_id |> string_of_int |> print_endline; client)

let () =  Lwt_main.run (sign_up "test2" "b" "c" "test2" >|= string_of_bool >|= print_endline )
let client2 =  Lwt_main.run (log_in "test2" "b" >|= fun client -> client.session_id |> string_of_int |> print_endline; client)


let client1 =  Lwt_main.run (do_command client1 (Send_message ("test2", "just a test")))

let client2 =  Lwt_main.run (do_command client2 (Get_all_message ["test1"]) >|= (fun client -> client.current_conversation) >|= (List.iter print_endline))



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
  body