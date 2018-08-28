open OUnit2
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

let uri_string = "http://0.0.0.0:8080/"

let create_account () =
  begin
    let json_str =
    `Assoc([("username", `String "david");
          ("password", `String "password1")])
    |> Yojson.Basic.to_string in
    let output = Client.post ~body:(Cohttp_lwt.Body.of_string json_str)
    (Uri.of_string ((uri_string) ^ "create-account"))
    >>= fun (_, s_ret) ->
    s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
    let real_body = Lwt_main.run output in
    real_body
  end

let login () =
  begin
    let json_str =
    `Assoc([("username", `String "master_client");
          ("password", `String "123456")])
    |> Yojson.Basic.to_string in
    let output = Client.post ~body:(Cohttp_lwt.Body.of_string json_str)
    (Uri.of_string ((uri_string) ^ "login"))
    >>= fun (_, s_ret) ->
    s_ret |> Cohttp_lwt.Body.to_string >|= (fun s_ret -> s_ret) in
    let real_body = Lwt_main.run output in
    real_body
  end

let tests = [

"test_create_account_1" >::
(fun _ -> assert_equal (create_account()) "");

"test_create_login_1" >::
(fun _ -> assert_equal ((login()) <> "N") true);

]


let all_tests = "Final project tests" >::: tests

let _ = run_test_tt_main all_tests