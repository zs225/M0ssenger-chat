
(******************************************************************************
 *
 *  Clean all old tables and setup fresh new tables
 *
 *  Tables:
 *
 *  clients:
 *         columns: username password realname connectionpswd, all text not null
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

(* [db] is the connected database used for this project *)
let db = Data_base.db


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
                  realname TEXT NOT NULL,
                  connectionpswd TEXT NOT NULL,
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

