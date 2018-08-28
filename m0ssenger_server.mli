(******************************************************************************
 *
 *  Mossenger server that performs logical operations
 *
 *****************************************************************************)



(* [clients_sessions] holds all current sessions numbers of users *)
val clients_sessions: int list ref


(* [create_new_user client] adds the client to server database; return true for
 * success; false otherwise *)
val create_new_user: ClientHolder.client_info -> bool


(* [login username password] validates the [username] and [password]
 * if it is valid, return a session id; otherwise
 * raises:  [InvalidLoginException] if the login is invalid *)
val login: string * string -> int


(* [group_chat_with username targetusers msg] send message from username to the
 * group identified by set of people [targetusers] union [username] return true
 * for success; false otherwise *)
val group_chat_with:  string * string list * string -> bool


(* [get_group_msg_from username msgs_from_lst] returns message history of group
 * [username] [msgs_from_lst] and update notification table
 * raises:  [UserNotExistExceptionn] if the users group is invalid  *)
val get_group_msg_from: string -> string list -> (int * string) list

(* [unread_notifications user] returns the notification table of [user], which
 * id an association list of (group_id, num of unread messages) for all group
 * chat the [client] is ever in *)
val unread_notifications: string -> (string * int) list
