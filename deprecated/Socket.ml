open Sys
open Unix
open Lwt

let host = Unix.inet_addr_loopback (* 127.0.0.1 *)
let port = 6600
let max_pending_request = 10

let sock_write conn message =
 let fd, _ = conn in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (Lwt_io.write_line oc message) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  return ()


let rec sock_recv sock maxlen =
  let rec read_to acc =
    let str = Bytes.create maxlen in
    let get_str = fun () ->
      Lwt.catch
         (fun () ->
         Lwt_unix.with_timeout 1.0 (fun () ->
         Lwt_unix.recv sock str 0 maxlen []))
         (fun _ -> return 0) in
    get_str () >>= fun recvlen ->
    if recvlen = 0 then return (acc)
    else read_to (acc^(String.sub (Bytes.to_string str) 0 recvlen)) in
  read_to ""

(* should already connected *)
let sock_read sock =
   let rec read () =
     sock_recv sock 1024 >>= fun answer ->
     Lwt_io.write_line Lwt_io.stdout answer;
     if (answer = "exit") then return ()
     else Lwt_unix.sleep 1.0 >>= fun () -> read () in
   read ()

let connect_to_server sock =
  (Lwt_unix.sleep 2.0 >>= fun () ->
  Lwt_unix.connect sock @@ ADDR_INET(host, port)); sock

let process_client_sock conn =
  sock_write conn "test1">>= fun () ->
  Lwt_unix.sleep 2.0 >>= fun () ->
  sock_write conn "test2" >>= fun () ->
  Lwt_unix.sleep 2.0 >>= fun () ->
  sock_write conn "test3"; return ()

let create_server sock =
  Lwt_unix.setsockopt sock SO_REUSEADDR true;
  Lwt_unix.bind sock @@ ADDR_INET(host, port);
  Lwt_unix.listen sock max_pending_request;
  (* need thread and loops *)
  Lwt_unix.accept_n sock 10 >>= fun (lst,_) ->
  process_client_sock (List.hd lst)


let create_socket () = Lwt_unix.socket PF_INET SOCK_STREAM 0

let () =
    let server_sock = create_socket () in
    let client_sock = create_socket () in
        Thread.create (fun () -> create_server server_sock) ();
        Thread.create (fun () -> Lwt_main.run (sock_read (connect_to_server client_sock))) ();
        Lwt_main.run (sock_read (connect_to_server client_sock))