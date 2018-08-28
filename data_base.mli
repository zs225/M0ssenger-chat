

type database


(******************************************************************************
 *
 *  Database metadata and connection
 *
 *****************************************************************************)


(* [db] is the connected database, as configured by [meta_data] *)
val db: database


(******************************************************************************
 *
 *  Clean all old tables and setup fresh new tables
 *
 *****************************************************************************)


(* [set_up unit] add [clients messages status] tables *)
val set_up: unit -> database


(******************************************************************************
 *
 *  Clients table operations
 *
 *****************************************************************************)


(* [contains_client username] is whether the user [username] exists in the
 * clients table *)
val contains_client: string -> bool


(* [check_login username password] is whether the row with [username] [password]
 * exists in the clients table *)
val check_login: string -> string -> bool


(* [all_clients unit] is a list of all clients usernames in the clients table *)
val all_clients: unit -> string list


(* [get_client username] is [(username, password)] of
 * the client [username] in clients table
 * requires:  [username] exists in clients table *)
val get_client: string -> string * string


(* [add_client username password] add a new row
 * to the clients table
 * requires: username should be in 200 characters limit *)
val add_client: string -> string -> unit


(******************************************************************************
 *
 *  Messages table operations
 *
 *****************************************************************************)


(* [contains_chat user_key] whether there is a row with [user_key] as group_id
 * in messages table, which means there is at least one message in the group *)
val contains_chat: string -> bool


(* [get_chat user_key] all messages and timestamps of with group_id = [user_key]
 * in messages table, ordered in decreasing time and id: newest first *)
val get_chat: string -> (int * string) list


(* [add_chat user_key time msg] add a new row [auto id | user_key | time | msg ]
 * to the messsages table *)
val add_chat: string-> int -> string -> unit


(******************************************************************************
 *
 *  Status table operations
 *
 *****************************************************************************)


(* [contains_status client] is whether there is a row with [client] exists in
 * the status table, which means there is at least one status information about
 * [client] *)
val contains_status: string -> bool


(* [contains_chat_status client group_id] is whether there is a row with
 * [client] and [group_id] exists in the status table, which means the status
 * of [client] in group [group_id] exists in the status table *)
val contains_chat_status: string -> string -> bool


(* [get_status client] is all (group_id, num) associated with [client] in the
 * status table, which is the notification table of [client]: an
 * association list of (group_id, num of unread messages) for all group chat the
 * [client] is ever in *)
val get_status: string -> (string * int) list


(* [inc_stsatus client group_id] increases the un_read of row with
 * [client group_id] by 1 if the row exists; otherwise, add a new row for this
 * status with un_read = 1 *)
val inc_stsatus: string -> string -> unit


(* [clear_stsatus client group_id] clear the un_read of row with
 * [client group_id] to 0 if the row exists; otherwise, add a new row for this
 * status with un_read = 0 *)
val clear_stsatus: string -> string -> unit