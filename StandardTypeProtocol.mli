
(* [notif_table] is the notification table: a list of notifications, where each
 * one is group of users, number of unread messages, latest message and time
 * sent *)
type notif_table = (string list * int * string * int) list


(* [to_signup jstring] unpack the json string to client information
 * requires: [jstring] has format {username: 'username',password: 'password'} *)
val to_signup: string -> ClientHolder.client_info


(* [from_signup username password] pack sign upo information to json string *)
val from_signup: string -> string -> string


(* [to_login jstring] unpack the json string to username and password tuple
 * requires: [jstring] has format {username: 'username',password: 'password'} *)
val to_login: string -> string * string


(* [from_login username password] pack login information to json string*)
val from_login: string -> string -> string


(* [to_session_id jstring] unpack the json string to session_id
 * requires: [jstring] has format {seesion_id: id} *)
val to_session_id: string -> int


(* [from_session_id session_id] pack session_id to json string *)
val from_session_id: int -> string


(* [to_clients jstring] unpack the json string to list of clients names
 * requires: [jstring] has format [name1, name2,...] *)
val to_clients: string  -> string list


(* [from_clients client_lst] pack [client_lst] to json string *)
val from_clients: string list -> string


(* [to_send_message jstring] unpack the json string to [(user, group, msg)]
 * requires: [jstring] has format {username: 'usernmae', targetusers:
 * [user1, user2,...], msg: 'msg'} *)
val to_send_message: string -> string * string list * string


(* [from_send_message user group msg] pack message sending information to json
 * string *)
val from_send_message: string -> string list -> string -> string


(* [to_messages jstring] unpack the json string to messages list
 * requires: [jstring] has format {time_stamps: [10,...], msgs: ['msg',...]}*)
val to_messages: string -> string list


(* [from_messages msgs] pack [msgs] to json string *)
val from_messages: (int * string) list -> string


(* [to_notif jstring] unpack the json string to notification table
 * requires: [jstring] has format {senders: [user1,...], unread: 0,
 * last_message: 'last_message', last_time: 0} *)
val to_notif: string -> notif_table


(* [from_notif notification] pack [notification] to json string *)
val from_notif: (string * int) list -> string