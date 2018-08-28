(******************************************************************************
 *
 * Server controller: Recieveing requests and sending responses
 *
 *****************************************************************************)

open Lwt
open Cohttp
open Cohttp_lwt_unix

(* [get_api_requests] is get request *)
type get_api_requests

(* [post_api_requests] is post request *)
type post_api_requests

(* [run_server port] start the server at [port] *)
val run_server: int ->  unit


