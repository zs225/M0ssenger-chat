open ClientHolder
open MessageHolder
open StandardTypeProtocol

exception InvalidLoginException

exception UserNotExistException

let clients_sessions = ref []

(* [all_users_exist users] true if every user in [users] exists *)
let all_users_exist = List.for_all ClientHolder.contains_client

let create_new_user new_client =
  let name = new_client.username in
  if ClientHolder.contains_client name then false
  else
    begin
      if String.length name > 200 || String.contains name '#' then false
      else (ClientHolder.add_client new_client.username new_client; true)
    end


let login (username, password) =
  if ClientHolder.login_ok username password
  then let new_session_id = Random.int 1000000 in
    clients_sessions := (new_session_id::(!clients_sessions)); new_session_id
  else raise InvalidLoginException


let group_chat_with (username, targetusers, msg) =
  let real_msg = (username ^ ": " ^ msg) in
  if all_users_exist (username::targetusers) then begin
    MessageHolder.add_chat (username::targetusers) real_msg ;
    ClientHolder.unread_msg_added (username::targetusers);
    true
  end
  else false


let get_group_msg_from username msgs_from_lst =
  if not (all_users_exist (username::msgs_from_lst))
  then raise UserNotExistException
  else if not (MessageHolder.does_contain (username::msgs_from_lst)) then []
       else let msgs = MessageHolder.get_chat (username::msgs_from_lst) in
            ClientHolder.read username (username::msgs_from_lst); msgs

let unread_notifications user = Data_base.get_status user


