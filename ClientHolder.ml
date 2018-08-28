(* [client_key] is a username *)
type client_key = string

(* [notif] is an association list between group ids and number of messages
 * unread by the client*)
type notif = (string * int) list

(* [status_table] is hash_table holding the number of unread messages of a
 * client *)
type status_table = (string, notif) Hashtbl.t

(* [client_info] is a record containing user data *)
type client_info = {username: string;
                    password: string
                    }


(* [client_list] is a Hashmap of usernames to a list of [client_info].
 * Hashes to a list because in the case of a collision, [Hashtbl]
 * just covers up the first *)
type client_list = (client_key, client_info list) Hashtbl.t


let rw = Mutex.create ()


(* [contains_client c_k c_l] is true if [c_k] is in [c_l] *)
let contains_client (c_k : client_key) =
  Mutex.lock rw;
  let r = Data_base.contains_client c_k in
  Mutex.unlock rw;r


(* [get_client c_k c_l] is the [client_info] associated with username [c_k] *)
let get_client (c_k : client_key)  =
  Mutex.lock rw;
  let (username, password) =
      Data_base.get_client c_k in
  let r = {username = username;
          password = password} in
  Mutex.unlock rw; r

(* [login_ok c_k pswd c_l] is true if the username [c_k] and password [pswd]
 * are associated with the same user, and the user is in [c_l] *)
let login_ok (c_k : client_key) (pswd : string)  =
  Mutex.lock rw;
  let r = Data_base.check_login c_k pswd in
  Mutex.unlock rw; r


(* [add_client c_k c_i c_l] mutates [c_l] by adding a new [c_k],[c_i] binding
 * if [c_k] is already bound within [c_l] update the binding *)
let add_client (c_k : client_key) (c_i : client_info)=
  Mutex.lock rw;
  try
    Data_base.add_client c_i.username c_i.password;
    Mutex.unlock rw
  with _ -> Mutex.unlock rw


let unread (c_k : client_key) (group_key : string) =
  Mutex.lock rw;
  Data_base.inc_stsatus c_k group_key;
  Mutex.unlock rw


let unread_msg_added (users : string list) =
  let sorted_users = List.sort_uniq Pervasives.compare users in
  let group_key =
    List.fold_left (fun acc i -> acc ^ i ^ "#") "" sorted_users in
  List.iter (fun a -> unread a group_key) sorted_users

let read (c_k : client_key) (users : string list) =
  let sorted_users = List.sort_uniq Pervasives.compare users in
  let group_key =
    List.fold_left (fun acc i -> acc ^ i ^ "#") "" sorted_users in
  Mutex.lock rw;
  Data_base.clear_stsatus c_k group_key;
  Mutex.unlock rw;
