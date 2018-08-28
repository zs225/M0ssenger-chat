

let rw = Mutex.create ()

let group_id (users : string list) =
  let id = users
    |> List.filter (fun a -> (String.trim a) <> "")
    |> List.sort_uniq Pervasives.compare
    |> List.fold_left (fun acc i -> match acc with
                                    | None -> Some i
                                    | Some acc -> Some(acc ^ i ^ "#")) None in
  match id with
  | None -> ""
  | Some s -> s

(* [get_chat users msg_chain] returns the list of (timestamp,messages) sent
 * by the group of people in [users]
 * requires: [users] have a chat within [msg_chain] *)
let get_chat (users : string list) =
  Mutex.lock rw;
  let msgs = Data_base.get_chat (group_id users) in
  Mutex.unlock rw; msgs


(* [does_contain users msg_chain] is true if the users in [users] have a chat
 * recorded within [msg_chain] *)
let does_contain (users : string list) =
  Mutex.lock rw;
  let r = Data_base.contains_chat (group_id users) in
  Mutex.unlock rw; r


(* [add_chat users msg msg_chain] is a mutated [msg_list] where the
 * message [msg] is added to the chat associated with [users] *)
let add_chat (users : string list) (msg : string) =
  Mutex.lock rw;
  let time = Unix.time () in
  Data_base.add_chat (group_id users) (int_of_float time) msg;
  Mutex.unlock rw
