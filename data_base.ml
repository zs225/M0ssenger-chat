open Mysql

type database = dbd

let debug = true

(******************************************************************************
 *
 *  Database metadata and connection
 *
 *****************************************************************************)


(* [meta_data] is the database information we use for this project *)
let meta_data = {
  dbhost = Some "localhost";
  dbname = Some "3110_final_M0SSENGER";
  dbport = None;
  dbpwd = Some "a";
  dbuser = Some "newuser";
  dbsocket = None
}


(* [db] is the connected database, as configured by [meta_data] *)
let db = connect meta_data


(******************************************************************************
 *
 *  Clean all old tables and setup fresh new tables
 *
 *  Tables:
 *
 *  clients:
 *         columns: username password, all text not null
 *         primary key: username, 200 characters limit
 *         invariant: no duplicate [username]
 *
 *  messages:
 *         columns: id(int) group_id(text) time(int) message(text)
 *         primary key: id, automatically increses
 *
 *  status:
 *         columns: id(int) group_id(text) username(text) un_read(int)
 *         primary key: id, automatically increses
 *         invariant: no duplicate combination of [group_id] [username]
 *                    unread >= 0
 *
 *****************************************************************************)


(* [clean unit] drop tables named [clients messages status] *)
let clean () =
  try
    let _ = exec db "DROP TABLE clients;" in
    let _ = exec db "DROP TABLE messages;" in
    ignore(exec db "DROP TABLE status;")
  with _ ->
  try
    let _ = exec db "DROP TABLE messages;" in
    ignore(exec db "DROP TABLE status;")
  with _ -> ignore(exec db "DROP TABLE status;")


(* [set_up unit] add [clients messages status] tables *)
let rec set_up () =
  try
    let _ = exec db "CREATE TABLE clients (
                  username VARCHAR(200) NOT NULL,
                  password TEXT NOT NULL,
                  PRIMARY KEY(username)
                );"     in
    let _ = exec db "CREATE TABLE messages (
                  id INT NOT NULL AUTO_INCREMENT,
                  group_id TEXT NOT NULL,
                  time INT NOT NULL,
                  message TEXT NOT NULL,
                  PRIMARY KEY (id)
                );"     in
    let _ = exec db "CREATE TABLE status (
                  id INT NOT NULL AUTO_INCREMENT,
                  group_id TEXT NOT NULL,
                  username TEXT NOT NULL,
                  un_read INT NOT NULL,
                  PRIMARY KEY (id)
                );"     in
          db
  with _ -> () |> clean |> set_up



(******************************************************************************
 *
 *  Clients table operations
 *
 *****************************************************************************)


let clean str = real_escape db str


let contains_client username =
   let result_cursor = exec db ("SELECT * FROM clients WHERE clients.username =
      \""^clean username^"\";") in
   (result_cursor |> size |> ml642int) <> "0"


let check_login username password  =
    let result_cursor = exec db ("SELECT * FROM clients WHERE clients.username =
      \""^clean username^"\" and clients.password =\""^clean password^"\";") in
    (result_cursor |> size |> ml642int) <> "0"


let all_clients () =
   let result_cursor = exec db ("SELECT * FROM clients;") in
  Mysql.map result_cursor (function
                           | [| Some username; Some _ |] ->
                                username
                           | _ -> (if debug
                                   then print_endline "username missing");"")


let get_client username =
   let result_cursor = exec db ("SELECT * FROM clients WHERE clients.username =
        \""^clean username^"\";") in
    match fetch result_cursor with
    | Some [| Some username; Some password|] ->  (username, password)
    | _ -> (if debug
            then print_endline "client does not exists, should not happen");
            ("","")


let add_client username password =
    ignore(exec db
    ("INSERT INTO clients (username, password)
      VALUES ( \""^username^"\",\""^password^"\");"))


(******************************************************************************
 *
 *  Messages table operations
 *
 *****************************************************************************)


let contains_chat user_key =
  let result_cursor = exec db ("SELECT * FROM messages WHERE messages.group_id =
    \""^clean user_key^"\" ORDER BY time DESC;") in
  (result_cursor |> size |> ml642int) <> "0"


let get_chat user_key =
  let result_cursor = exec db ("SELECT * FROM messages WHERE messages.group_id =
    \""^clean user_key^"\" ORDER BY time DESC;") in
  Mysql.map result_cursor (function
                           | [| Some _; Some _; Some time; Some  message|] ->
                                (int_of_string time, message)
                           | _ -> (if debug then
                                  print_endline "time message missing");(0, ""))


let add_chat user_key time msg =
  ignore(exec db ("INSERT INTO messages (group_id, time, message)
          VALUES ( \""^user_key^"\","^(string_of_int time)^", \""^msg^"\");"))


(******************************************************************************
 *
 *  Status table operations
 *
 *****************************************************************************)


let contains_status client =
    let result_cursor = exec db ("SELECT * FROM status WHERE status.username =
          \""^clean client^"\";") in
    (result_cursor |> size |> ml642int) <> "0"


let contains_chat_status client group_id =
    let result_cursor = exec db ("SELECT * FROM status WHERE status.username =
          \""^clean client^"\" and status.group_id = \""^group_id^"\";") in
    (result_cursor |> size |> ml642int) <> "0"


let get_status client =
    let result_cursor = exec db ("SELECT * FROM status WHERE status.username =
          \""^clean client^"\";") in
    map result_cursor (function
                       |[| Some _; Some group_id; Some _; Some num|] ->
                           (group_id, int_of_string num )
                       | _ -> (if debug then
                              print_endline "group_id unread missing");("", 0))


(* [add_status client group_id] add a new row to the status table; should not be
 * used from outside, in order to maintain invariant *)
let add_status client group_id num =
    ignore(exec db ("INSERT INTO status (group_id, username, un_read) VALUES
      ( \""^clean group_id^"\",\""^clean client^"\","^(string_of_int num)^");"))


let inc_stsatus client group_id =
    if contains_chat_status client group_id
    then ignore(exec db ("UPDATE status SET un_read = un_read + 1
                          WHERE status.username = \""^clean client^"\"and
                                status.group_id =\""^clean group_id^"\";"))
    else add_status client group_id 1


let clear_stsatus client group_id =
   if contains_chat_status client group_id
   then ignore(exec db ("UPDATE status SET un_read = 0
                         WHERE status.username = \""^clean client^"\"and
                               status.group_id =\""^clean group_id^"\";"))
   else add_status client group_id 0
