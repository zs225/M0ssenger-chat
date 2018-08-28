open Lwt
open Cohttp
open Cohttp_lwt_unix
open StandardTypeProtocol
open M0ssenger_server

type post_api_requests =
  | Bad_request_post
  | Unauthorized_request_post
  | Create
  | Login
  | Groupchat

type get_api_requests =
  | Bad_request_get
  | Unauthorized_request_get
  | Get_group_msg_from of string * (string list)
  | See_clients
  | Self_status of string


(* [debug] the flag indicating debugging mode  *)
let debug = true


(* [handle_exception e] returns "N". In the debugging mode, it will print out
 * the error message *)
let handle_exception e =
    if not debug then "N" else (e |> Printexc.to_string |> print_endline; "N")



(* [uri_parser_get uri] returns a type of [get_api_request]*)
let uri_parser_get uri =
  try
    let clean_uri = String.sub uri 15 ((String.length uri) - 15) in
    match clean_uri with
    | x ->
      begin
        let lst = String.split_on_char '?' x in
        match lst with
        | "see-clients"::tl ->
          begin
            let sess = List.hd tl in
            let sess_id = String.sub sess 11 ((String.length sess) - 11)
              |> Pervasives.int_of_string in
            if List.exists (fun a -> a = sess_id) (!clients_sessions) then
              See_clients
            else
              Unauthorized_request_get
          end
        | "getmsgs"::tl ->
          begin
            let uri_info = List.hd tl in
            let info_lst = String.split_on_char '&' uri_info in
            match info_lst with
            | sess::username::tl2 ->
              begin
                let sess_id = String.sub sess 11 ((String.length sess) - 11)
                  |> Pervasives.int_of_string in
                let username_id =
                  Uri.pct_decode (String.sub username 9
                                              ((String.length username) - 9)) in
                let other_lst =
                  List.map (fun a -> Uri.pct_decode
                                     (String.sub a 10 ((String.length a) - 10)))
                  tl2 in
                if List.exists (fun a -> a = sess_id) (!clients_sessions) then
                  Get_group_msg_from(username_id, other_lst)
                else
                  Unauthorized_request_get
              end
            | _ -> Bad_request_get
          end
        | "self-status"::tl ->
          begin
            let uri_info = List.hd tl in
            let info_lst = String.split_on_char '&' uri_info in
            match info_lst with
            | sess::username::tl2 ->
              begin
                let sess_id = String.sub sess 11 ((String.length sess) - 11)
                  |> Pervasives.int_of_string in
                let username_id =
                  Uri.pct_decode (String.sub username 9
                                 ((String.length username) - 9)) in
                if List.exists (fun a -> a = sess_id) (!clients_sessions) then
                  Self_status username_id
                else
                  Unauthorized_request_get
              end
            | _ -> Bad_request_get
          end
        | _ -> Bad_request_get
      end
  with
  | _ -> Bad_request_get


(* [uri_parser_post uri] returns a type of [post_api_request]*)
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
              Groupchat
            else
              Unauthorized_request_post
          end
        | _ -> Bad_request_post
      end
  with
  | _ -> Bad_request_post


(* [handle_api_request_post req body]
 * creates a server response based on the [req] type and what is in [body]*)
let handle_api_request_post (req : post_api_requests) body =
  match req with
  | Unauthorized_request_post ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      ("Unauthorized Request"))
        >>= (fun body -> Server.respond_string ~status:`Unauthorized ~body ())
    end
  | Bad_request_post ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      ("Bad Request"))
        >>= (fun body -> Server.respond_string ~status:`Not_found ~body ())
    end

  | Create ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      try
        body |> to_signup |> create_new_user
      with e -> ignore(handle_exception e); false)
      >>= (fun success ->
      let status = if success then `Created else `Forbidden in
      Server.respond_string ~status: status ~body:"" ())
    end

  | Login ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      try
        body |> to_login |> login |> from_session_id
      with e -> handle_exception e ) >>= (fun result ->
      let status = if result = "N" then `Unauthorized else `Accepted in
      Server.respond_string ~status: status ~body: result ())
    end

  | Groupchat ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      try
         body |> to_send_message |> group_chat_with
      with e -> ignore(handle_exception e); false) >>= (fun success ->
      let status = if success then `Accepted else `Not_found in
      Server.respond_string ~status: status ~body:"" ())
    end

(* [handle_api_request_get req body]
 * creates a server response based on the [req] type and what is in [body]*)
let handle_api_request_get (req : get_api_requests) body =
  match req with
  | Unauthorized_request_get ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      ("Unauthorized Request"))
        >>= (fun body -> Server.respond_string ~status:`Unauthorized ~body ())
    end
  | Bad_request_get ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      ("Bad Request"))
        >>= (fun body -> Server.respond_string ~status:`Not_found ~body ())
    end
  | Get_group_msg_from(username, from_user_lst) ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      try
        (get_group_msg_from username from_user_lst) |> from_messages
      with e -> handle_exception e)
      >>= (fun body -> if body = "N" then
        Server.respond_string ~status: `Not_found ~body ()
        else Server.respond_string ~status: `Accepted ~body ())
    end

  | See_clients ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      (from_clients (Data_base.all_clients ())))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
    end
  | Self_status username ->
    begin
      body |> Cohttp_lwt.Body.to_string >|= (fun _ ->
      try
        username |> unread_notifications |> from_notif
      with e -> handle_exception e) >>= (fun body ->
        if body = "N" then
        Server.respond_string ~status: `Not_found ~body ()
        else Server.respond_string ~status: `Accepted ~body ())
    end


(* [server port] is the server in [port] *)
let server port =
  let start_server _ http_request body =
    let api_req = http_request |> Request.uri |> Uri.to_string in
    let post_or_get = http_request |> Request.meth |> Code.string_of_method in
    if post_or_get = "GET" then
      begin
        let api_req_final = api_req |> uri_parser_get in
        handle_api_request_get api_req_final body
      end
    else
      begin
        let api_req_final = api_req |> uri_parser_post in
        handle_api_request_post api_req_final body
      end
  in
  Server.create ~mode:(`TCP (`Port port))
    (Server.make ~callback:start_server ())

let run_server port = Lwt_main.run (Random.self_init (); server port)
