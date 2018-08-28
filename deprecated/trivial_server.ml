open Lwt
open Cohttp
open Cohttp_lwt_unix
open ClientHolder
open MessageHolder
open Yojson.Basic.Util

type post_api_requests =
  | Bad_request_post
  | Unauthorized_request_post
  | Create
  | Login
  | Chat

type get_api_requests =
  | Bad_request_get
  | Unauthorized_request_get
  | Get_msg_from of string * string
  | See_clients

let rw = Lwt_mutex.create ()

let clients_hashtbl = ClientHolder.empty_client_list

let msg_hashtbl = MessageHolder.empty_msg_list

let clients_usernames = ref ["master client"]

let () = Random.self_init ()

let clients_sessions = ref []

(* [to_string_clients client_list] returns a string representing the json
 * representation of [clinet_list] *)
let to_string_clients client_list =
  let json_lst = List.fold_left (fun acc i -> (`String i)::acc) [] client_list
  in
  `Assoc([("clients", `List json_lst)]) |> Yojson.Basic.to_string

(* [create_new_user body] adds the user represented by [body] to clients_hashtbl
 * and clients_usernames.  Or does nothing*)
let create_new_user body =
  try
    let info = Yojson.Basic.from_string body in
    let new_client = {
    username = to_string (member "username" info);
    password = to_string (member "password" info);
    realname = to_string (member "realname" info);
    connectionpswd = to_string (member "connectionpswd" info)} in
    if ClientHolder.contains_client new_client.username clients_hashtbl then
      begin
        "N"
      end
    else
      begin
        ClientHolder.add_client new_client.username new_client clients_hashtbl;
        clients_usernames := (new_client.username::(!clients_usernames));
        "Y"
      end
  with
  | _ -> "N"

(* [login body] validates the username and password information in [body]
 * and creates a new client session.*)
let login body =
  try
    let info = Yojson.Basic.from_string body in
    let username = to_string (member "username" info) in
    let password = to_string (member "password" info) in
    if ClientHolder.login_ok username password clients_hashtbl then
      begin
        let new_session_id = Random.int 1000000 in
        clients_sessions := (new_session_id::(!clients_sessions));
        let session_json = `Int(new_session_id) in
        let return_json = `Assoc([("session_id", session_json)]) in
        Yojson.Basic.to_string return_json
      end
    else
      begin
        "N"
      end
  with
  | _ -> "N"

(* [chat_with body] adds to a chat record in [msg_hashtbl] *)
let chat_with body =
  try
    let info = Yojson.Basic.from_string body in
    let username = to_string (member "username" info) in
    let targetuser = to_string (member "targetuser" info) in
    let msg = to_string (member "msg" info) in
    let real_msg = (username ^ ": " ^ msg) in
    if ClientHolder.contains_client targetuser clients_hashtbl then
      begin
        MessageHolder.add_chat [username; targetuser] real_msg msg_hashtbl;
        "Y"
      end
    else
      begin
        "N"
      end
  with
  | _ -> "N"

(* [get_msg_from username msgs_from] returns a json string of the messages
 * between [username] and [megs_from] *)
let get_msg_from username msgs_from =
  try
    if ClientHolder.contains_client username clients_hashtbl then
      begin
        if MessageHolder.does_contain [username; msgs_from] msg_hashtbl then
          begin
            let msgs =
              MessageHolder.get_chat [username; msgs_from] msg_hashtbl in
            let time_stamps =
              List.fold_left (fun acc i -> `Float(fst i)::acc) [] msgs
              |> List.rev in
            let json_msg =
              List.fold_left (fun acc i -> `String(snd i)::acc) [] msgs
              |> List.rev in
            `Assoc ([("time_stamps", `List(time_stamps));
              ("msgs", `List(json_msg))])
              |> Yojson.Basic.to_string
          end
        else
          begin
            let empty_json_lst = `List([]) in
            `Assoc ([("time_stamps", empty_json_lst); ("msgs", empty_json_lst)])
            |> Yojson.Basic.to_string
          end
      end
    else
      begin
        "N"
      end
  with
  | _ -> "N"

(* [uri_parser_get uri] *)
let uri_parser_get uri =
  try
    let clean_uri = String.sub uri 15 ((String.length uri) - 15) in
    match clean_uri with
    | "see-clients" -> See_clients
    | x ->
      begin
        let lst = String.split_on_char '?' x in
        match lst with
        | "getmsg"::tl ->
          begin
            let uri_info = List.hd tl in
            let info_lst = String.split_on_char '&' uri_info in
            let sess = List.nth info_lst 0 in
            let sess_id = String.sub sess 11 ((String.length sess) - 11)
              |> Pervasives.int_of_string in
            let username = List.nth info_lst 1 in
            let username_id = String.sub username 9 ((String.length username) - 9) in
            let msgs_from = List.nth info_lst 2 in
            let msgs_from_id = String.sub msgs_from 10 ((String.length msgs_from) - 10) in
            if List.exists (fun a -> a = sess_id) (!clients_sessions) then
              Get_msg_from(username_id, msgs_from_id)
            else
              Unauthorized_request_get
          end
        | _ -> Bad_request_get
      end
  with
  | _ -> Bad_request_get

(* *)
let uri_parser_post uri =
  try
    let clean_uri = String.sub uri 15 ((String.length uri) - 15) in
    match clean_uri with
    | "create-account" -> Create
    | "login" -> Login
    | x ->
      begin
        let lst = String.split_on_char '?' x in
        match lst with
        | "chat"::tl ->
          begin
            let sess = List.hd tl in
            let sess_id = String.sub sess 11 ((String.length sess) - 11)
            |> Pervasives.int_of_string in
            if List.exists (fun a -> a = sess_id) (!clients_sessions) then
              Chat
            else
              Unauthorized_request_post
          end
        | _ -> Bad_request_post
      end
  with
  | _ -> Bad_request_post

(* *)
let handle_api_request_post (req : post_api_requests) body =
  match req with
  | Unauthorized_request_post ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      ("Unauthorized Request"))
        >>= (fun body -> Server.respond_string ~status:`Unauthorized ~body ())
    end
  | Bad_request_post ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      ("Bad Request"))
        >>= (fun body -> Server.respond_string ~status:`Not_found ~body ())
    end
  | Create ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (create_new_user body))
      >>= (fun body -> if body = "Y" then
        Server.respond_string ~status: `Created ~body ()
        else Server.respond_string ~status: `Forbidden ~body ())
    end
  | Login ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (login body))
      >>= (fun body -> if body = "N" then
        Server.respond_string ~status: `Unauthorized ~body ()
        else Server.respond_string ~status: `Accepted ~body ())
    end
  | Chat ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (chat_with body))
      >>= (fun body -> if body = "N" then
        Server.respond_string ~status: `Not_found ~body ()
        else Server.respond_string ~status: `Accepted ~body ())
    end

let handle_api_request_get (req : get_api_requests) body =
  match req with
  | Unauthorized_request_get ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      ("Unauthorized Request"))
        >>= (fun body -> Server.respond_string ~status:`Unauthorized ~body ())
    end
  | Bad_request_get ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      ("Bad Request"))
        >>= (fun body -> Server.respond_string ~status:`Not_found ~body ())
    end
  | Get_msg_from(username, from_user) ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (get_msg_from username from_user))
      >>= (fun body -> if body = "N" then
        Server.respond_string ~status: `Not_found ~body ()
        else Server.respond_string ~status: `Accepted ~body ())
    end
  | See_clients ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      (to_string_clients (!clients_usernames)))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    end

let server =

  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in

    let meth = req |> Request.meth |> Code.string_of_method in

    if meth = "GET" then
      begin
        let api_request = uri |> uri_parser_get in
        handle_api_request_get api_request body
      end
    else
      begin
        let api_request = uri |> uri_parser_post in
        handle_api_request_post api_request body
      end
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)