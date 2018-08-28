open ClientHolder
open Yojson.Basic.Util

type notif_table = (string list * int * string * int) list


(******************************************************************************
 *
 * Helper methods to parse fields and lists
 *
 *****************************************************************************)

(* [parse j name f] the value has name [name] in the json tree [j]
* requires:
 *        - [j] is a jason object and has attribute [name] *)
let parse j name f =
  j |> member name |> f

(* [list_to_parsed parse_one fold init j] the parsed data sctucture by parsing
 * each element in the jason list [j] using [parse_one], and combine them using
 * [fold] and [init]
 * requires:
 *        - [j] is a jason list *)
let list_to_parsed parse_one fold init j =
  j |> to_list |> List.map parse_one |> List.fold_left fold init

(* [list_to_list f j] the parsed list by parsing each element in the jason
 * list [j] using [f], and combine them to a list
 * requires:
 *        - [j] is a jason list *)
let list_to_list f =
  list_to_parsed f (fun acc elem -> elem::acc) []

(* [list_to_list f j] the parsed list by parsing each element in the jason
 * list by converting it directly to string, and combine them to a list
 * requires:
 *        - [j] is a jason list of strings *)
let list_to_string_list =
  list_to_list to_string

(******************************************************************************
 *
 * parse to and pack from standard type
 *
 *****************************************************************************)

let to_login jstring =
  let j = Yojson.Basic.from_string jstring in
  (parse j "username" to_string,  parse j "password" to_string)


let from_login user_name password =
  `Assoc([("username", `String(user_name));
          ("password", `String(password))])
  |> Yojson.Basic.to_string

let to_session_id jstring =
 let j = Yojson.Basic.from_string jstring in
  parse j "session_id" to_int


let from_session_id id =
  `Assoc([("session_id", `Int id)])
  |> Yojson.Basic.to_string


let to_signup jstring =
 let j = Yojson.Basic.from_string jstring in
 let parse_j field = parse j field to_string in
  {
    username = parse_j "username";
    password = parse_j "password"
  }

let from_signup user_name password =
  `Assoc([("username", `String(user_name));
          ("password", `String(password))])
  |> Yojson.Basic.to_string


(* returns a list of strings representing all clients in [jstring] *)
let to_clients jstring =
  let j = Yojson.Basic.from_string jstring in
  parse j "clients" (list_to_list to_string)

(* [from_clients client_list] returns a string representing the json
 * representation of [client_list] *)
let from_clients client_list =
  let json_lst = List.fold_left (fun acc i -> (`String i)::acc) [] client_list
  in
  `Assoc([("clients", `List json_lst)]) |> Yojson.Basic.to_string

let to_send_message jstring =
   let j = Yojson.Basic.from_string jstring in
   (parse j "username" to_string,
    parse j "targetusers" (list_to_list to_string),
    parse j "msg" to_string)


(* a helper to pack send message json *)
let from_send_message username targetusers msg =
  `Assoc([("username", `String(username));
          ("targetusers", `List(List.map (fun x -> `String(x)) targetusers));
          ("msg", `String(msg)) ])
  |> Yojson.Basic.to_string



(* returns a list of strings to outputs as the chat history
 * input [jstring] is {times: float list, msgs: string list*)
let to_messages jstring =
  let json = Yojson.Basic.from_string jstring in
  let messages = json |> member "msgs" |> to_list in
    List.rev (List.fold_left (fun acc elt -> (to_string elt)::acc)
                              []
                              messages)


let from_messages msgs =
  let time_stamps =
    List.fold_left (fun acc i -> `Int(fst i)::acc) [] msgs
    |> List.rev in
  let json_msg =
    List.fold_left (fun acc i -> `String(snd i)::acc) [] msgs
    |> List.rev in
  `Assoc ([("time_stamps", `List(time_stamps));
    ("msgs", `List(json_msg))])
    |> Yojson.Basic.to_string


let to_notif jstring =
  let json = Yojson.Basic.from_string jstring in
  list_to_list (fun j ->
    (parse j "senders" (list_to_list to_string),
     parse j "unread" to_int,
     parse j "last_message" to_string,
     parse j "last_time" to_int)) json


let from_notif notif =
  let notif_lst =
  List.map (fun (s, num) ->
      let senders = s |> String.split_on_char '#'
                      |> List.filter (fun a -> (String.trim a) <> "") in
      let messages = MessageHolder.get_chat senders in
      let (time, msg) = if messages = [] then (0,"") else (List.hd messages) in
      let senders_json = List.map (fun a -> `String (a)) senders in
    `Assoc([("senders", `List(senders_json));
            ("unread", `Int(num));
            ("last_message", `String(msg));
            ("last_time", `Int(time))])) notif in
  `List(notif_lst) |> Yojson.Basic.to_string


