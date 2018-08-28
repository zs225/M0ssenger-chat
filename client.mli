open StandardTypeProtocol

(* A [client] stores information needed in a networking communication:
 * e.g. user name, session id, new conversation buffer, updating thread etc *)
type client


(* [conversation_update_buffer] is the syncrynized buffer of new conversation
 * pulled from server and waited to e displayed *)
type conversation_update_buffer


(* [command] is a possible command issued by app controller. Bellow are just
 * seceral examples *)
type command = Send_message of string list * string (* other users, messaged *)
              | Get_all_message of string list
              (* set the message buffer to from version n to current *)
              | Get_messages_diff_since of (string list) * int


type info_kind = User_name | Session_id


(******************************************************************************
 *
 * Accessing information in client
 *
 *****************************************************************************)


(* [current_conversation_buffer client] the buffer used by [client] to obtain
 * its recent new conversation pulled from server and waited to be displayed *)
val conversation_update_buffer: client -> conversation_update_buffer


(* [messages_diff_to_display buffer] take out messages bufferred in [buffer]:
 * get all messages in buffer and clean buffer *)
val messages_diff_to_display: conversation_update_buffer -> string list


(* [peek_messages_diff buffer] peek message bufferred in [buffer] but not clean
 * buffer *)
val peek_messages_diff: conversation_update_buffer -> string list


(* [peek_messages_people_in_chat buffer] peek people in the chat tracked by the
 * buffer but not clean buffer *)
val peek_messages_people_in_chat: conversation_update_buffer -> string list


(* [get_info client info_kind] retrieves the information of kind [info_kind]
 * from client *)
val get_info: info_kind -> client -> string

(******************************************************************************
 *
 * Operations that can be done without granted a client
 *
 *****************************************************************************)


(* [log_in user_name password] performs a log-in if [user_name] corresponds to
 * [password] *)
val log_in: string -> string -> client Lwt.t


(* [sign_up user_name password] creates an account with a [user_name]
 * and [password] *)
val sign_up: string -> string -> bool Lwt.t


(******************************************************************************
 *
 * Operations that must be done with a client
 *
 *****************************************************************************)


(* [get_clients client ] retrieve all clients registered in server *)
val get_clients: client -> string list Lwt.t


(* [do_command c command] is a client after [c] executes the [command] *)
val do_command: client -> command -> client Lwt.t

(* [update_with_freq_f client f] start to update the conversation in frequency f
 * from the server in a seperate thread. If there is already a thread doing this
 * , that thread will be killed and a new one will be started *)
val update_with_freq_f: client -> float -> client

(* [stop_update_and_clean_up client] stop the updating thread and clean up the
 * buffer *)
val stop_update_and_clean_up: client -> client

(******************************************************************************
 *
 * Operations that must be done with a client, return a information
 *
 *****************************************************************************)


(* [get_notification_table client] get the updated notification table, which is
 * a map from users in chat to num of unread messages. All chats are in cluded in
 * the table, even if the num of unread is 0 *)
val get_notification_table: client -> notif_table Lwt.t

(* [get_notification_abbreiviation client] get the abbreviated version of
 * notification, which is sum of unread message number and set of people who
 * send those messages *)
val get_notification_abbreiviation: client -> (int * string list)  Lwt.t

