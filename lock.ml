open Lwt
type t = {
  num_of_readers: int ref ;
  there_is_writer: bool ref ;
  mutex: Mutex.t;
  read_condition: Condition.t;
  write_condition: Condition.t
  }

let debug_lock = false

let create_lock () =
  {
    num_of_readers =  ref 0 ;
    there_is_writer =  ref false;
    mutex = Mutex.create ();
    read_condition = Condition.create ();
    write_condition =  Condition.create ();
  }

let enter_write lock =
  let id = Thread.self () |> Thread.id |> string_of_int in
  Mutex.lock lock.mutex;
  if debug_lock then print_endline (id^" enter_write lock "^
                    (string_of_bool !(lock.there_is_writer)));
  while (!(lock.there_is_writer) || !(lock.num_of_readers) > 0) do
    (*if debug_lock then print_endline (id^" wait to write");*)
    Condition.wait lock.write_condition lock.mutex
  done;
  if debug_lock then print_endline (id^" wait done");
  lock.there_is_writer := true;
  if debug_lock then print_endline (id^" to unlock");
  Mutex.unlock lock.mutex;
  if debug_lock then print_endline (id^" enter_write unlock")

let leave_write lock =
  let id = Thread.self () |> Thread.id |> string_of_int in
  Mutex.lock lock.mutex;
  if debug_lock then print_endline (id^" leave_write lock");
  lock.there_is_writer := false;
  Condition.broadcast lock.read_condition;
  Condition.signal lock.write_condition;
  Mutex.unlock lock.mutex;
  if debug_lock then print_endline (id^" leave_write unlock")


let enter_read lock =
  let id = Thread.self () |> Thread.id |> string_of_int in
  Mutex.lock lock.mutex;
  if debug_lock then print_endline (id^" enter_read lock");
  while !(lock.there_is_writer) do
    Condition.wait lock.read_condition lock.mutex
  done;
  lock.num_of_readers := !(lock.num_of_readers) + 1;
  Mutex.unlock lock.mutex;
  if debug_lock then print_endline (id^" enter_read unlock")

let leave_read lock =
  let id = Thread.self () |> Thread.id |> string_of_int in
  Mutex.lock lock.mutex;
  if debug_lock then print_endline (id^" leave_read lock");
  lock.num_of_readers := !(lock.num_of_readers) - 1;
  (if !(lock.num_of_readers) = 0 then Condition.signal lock.write_condition);
  Mutex.unlock lock.mutex;
  if debug_lock then print_endline (id^" leave_read unlock")

