(* This file is responsible for what is actually output to the command line
 * interface *)

(* [disp_help] prints a block of text informing the user about how to
 * use the app *)
let disp_help () =
  print_endline "\n---------------------[HELP BLOCK]--------------------------";
  print_endline "Here is a list of commands that the app can recognize";
  print_endline "[Note: all commands must be preceeded by a \":\"]\n";

  print_endline "  :login  - choose log in option";
  print_endline "  :create - choose create account option";
  print_endline " ";
  print_endline "  :help   - displays this text block";
  print_endline "  :emojis - displays a list of supported emojis";
  print_endline "  :exit   - close out of the app";
  print_endline " ";
  print_endline ("  :look   - display a list of other online users and number\n"
                ^"            of overall unread messages\n");

  print_endline ("  :chats  - displays the chats you are a part of, as well\n"
                ^"            as unread messages per chat/recent message preview\n");

  print_endline "  :logout - return to the log in/create account screen\n";

  print_endline "  :back   - back out of a chat\n";

  print_endline ("  :message [user1] [user2] ... \n"
                ^"          - begin a chat with [user1] [user2] ...");


  print_endline "------------------------------------------------------------\n"


let disp_emojis () =
  print_endline "\n---------------------[HELP BLOCK]------------------------";
  print_endline ("Supported emojis (emojis are recognized in the form\n"
                ^"\":emoji_ex:\": ");
  print_endline (":heart:    :yay:       :lmao:        :rofl:      :wink:  \n"
                ^":fml:      :angry:     :poop:        :flip_off:  :high_five:\n"
                ^":thinking: :thumbs_up: :thumbs_down: :clap:      :eyes:\n"
                ^":tongue:   :zzz:       :bomb:        :monkey:    :snake:\n"
                ^":whale:    :flower:    :eggplant:    :beer:      :star:\n"
                ^":fire:     :balloon:   :smile:       :frown:     :wow:\n"
                ^":water:\n"
                ^"\n"
                ^"Emojis that aren't completely obvious by name:\n"
                ^":cool:        - face with sunglasses\n"
                ^":love_u:      - face with heart-eyes\n"
                ^":ok:          - ok hand gesture\n"
                ^":fist:        - raised fist in the air\n"
                ^":bump:        - oncoming fist (fistbump style)\n"
                ^":jesus:       - hands in prayer position\n"
                ^":smooch:      - kiss mark\n"
                ^":heartbroken: - broken heart\n"
                ^":b:           - red B (for the memers)\n");
  print_endline "------------------------------------------------------------\n"

let disp_invalid_username () =
  print_endline "Wtf you can't have an empty username"

let disp_msg_init_error () =
  print_endline "Seems like you got somebody's username wrong"

(* [disp_init] prints the initial message displayed to the user upon starting
 * the app *)
let disp_init () =
  print_endline "\n\n*******************************************************";
  print_endline "                 WELCOME TO M0SSENGER";
  print_endline "*******************************************************\n";
  print_string ("Log in or create an account \n"
              ^"(type\":help\" for a quick overview of commands):\n>")

let disp_account_creation () =
  print_endline "\n\n===========================================================";
  print_endline "                 New Account Creation";
  print_endline "===========================================================";

  print_endline ("NOTE: If you typed \":create\" "
                ^"by accident, you can type the command \":back\" "
                ^"(in the username field ONLY) to get back to the "
                ^"previous screen\n")

let disp_bad_cmd () =
  print_endline "That command doesn't make sense to execute here."

let disp_login_fail () =
  print_endline "Shit dude.  Looks like that password and username don't match";
  print_string "Try logging in again or creating an account: \n>"

let disp_login_success user =
  print_endline ("User ["^user^"] authorized.")

let disp_app_menu () =
  print_endline "\n\n*******************************************************";
  print_endline "                    ---MAIN MENU---";
  print_endline "*******************************************************\n";
  print_endline "Welcome to the app :O  Here are a few things you can try doing:";
  print_endline " - view users of the app + overall unread messages";
  print_endline " - view chats, specific unreads";
  print_endline " - enter a chat\n";
  print_endline "*******************************************************\n"


let disp_create_fail () =
  print_endline "\nMan, looks like that username is invalid/taken.";
  print_string "Try again if you want: \n>"

let disp_create_success () =
  print_endline "Account creation successful!";
  print_string "Why don't you try logging in with that fancy new account you made: "

(* requires:
 * [users] is a list of valid usernames (should be screened before
 * entering messaging mode) *)
let disp_msg_intro users =
  let rec transform_users_to_string lst acc =
    match lst with
    | [] -> acc
    (* Case where you are the only one in the chat *)
    | h::[] when acc="" -> h
    (* Last person in a string of names *)
    | h::[] ->  acc^"and "^h
    (* General case *)
    | h::t -> transform_users_to_string t (acc^h^", ")
  in

  let chatroom_str = transform_users_to_string
      (users |> List.sort_uniq Pervasives.compare) "" in
    print_endline ("\nYou are now messaging: "^chatroom_str);
    print_endline ""

(* Lists out everybody who is currently connected as a list of username
 * requires: [lst] is a string list of usernames *)
let disp_clients_online lst =
  if List.length lst = 0
  then print_endline "There are currently no other users connected :("
  else begin
    print_endline "------------------------------------------------";
    print_endline "               Users of the app:";
    print_endline "------------------------------------------------";
    List.iter print_endline lst
  end

let disp_notif (num, lst) =
  print_endline "------------------------------------------------";
  print_endline "              Unread messages:";
  print_endline "------------------------------------------------";
  print_string ((Pervasives.string_of_int num) ^ "\n");
  print_endline "------------------------------------------------";
  print_endline "         You have unread messages from:";
  print_endline "------------------------------------------------";
  List.iter (fun a -> print_string (a ^ "\n")) lst


(* Lists out everybody who is currently connected as a list of username
 * requires: [lst] is a string list of usernames *)
let disp_all_chats lst =
  if List.length lst = 0
  then print_endline "There are currently no chats you involved in :("
  else begin
    let group_display_limit = 10 in
    print_endline "------------------------------------------------";
    print_endline "            Chats you are in :";
    print_endline "------------------------------------------------";
    List.iter (fun (group, num, msg, time) ->
      let group_string_opt = List.fold_left (fun acc one ->
                                            match acc with
                                            | None -> Some one
                                            | Some s -> Some (one^","^s)
                                        ) None group in
      match group_string_opt with
      | None -> failwith "impossible";
      | Some group_string ->
      let length = String.length group_string in
      let group_brev = if length >  group_display_limit
                       then (String.sub group_string 0 group_display_limit)^".."
                       else group_string^(String.make (2 + group_display_limit - length) ' ') in
      let current_time = Unix.time () in
      let diff_t  = (current_time -. (float_of_int time)) /. 86400. in
      let tm = Unix.localtime (float_of_int time) in
      let time_string =
        if diff_t < 1.
        then (string_of_int tm.tm_hour)^":"^(string_of_int tm.tm_min)
        else if diff_t < 31536000.
        then (string_of_int tm.tm_mon)^"/"^(string_of_int tm.tm_mday)
        else string_of_int tm.tm_year in
      print_endline ("("^(string_of_int num)^") "^group_brev^"  "^time_string^" "^msg);
      print_endline "------------------------------------------------"
    ) lst
  end


(* Displays the message history as represented by [msgs]
 * Called when opening a chat with anybody.
 *
 * requires:
 * [msgs] is a list of username, message tuples
 * The elements of [msgs] are presented in the order they are to be printed
 * Each element of [msg] represents a single message sent by a user. The list
 * is there to break up long messages
 *
 * TODO - should this only display like the past 500 messages or something *)
let disp_convo_history msgs =
  List.iter print_endline msgs

(* Used so that the command/message the user typed in isn't shown twice *)
let erase_user_input () =
  ANSITerminal.move_cursor 0 (-1);
  ANSITerminal.erase (Below)

(* Erases entire screen and sets the cursor to the top left corner
 * used mainly with the :clear command *)
let clean_screen () =
  ANSITerminal.erase ANSITerminal.Screen;
  ANSITerminal.set_cursor 1 1

(* HELPER - THIS IS WHERE WE HARDCODE OUR EMOJI STRINGS
 * A few hard-coded emoji values
 * requires: [emoji] should actually be a user typed emoji *)
let get_emoji emoji =
  match emoji with
  (* TODO - add more hard codings to your heart's content *)
   | ":heart:" -> Emoji.beating_heart
   | ":yay:" -> Emoji.grinning_face_with_smiling_eyes
   | ":lmao:" -> Emoji.face_with_tears_of_joy
   | ":rofl:" -> Emoji.rolling_on_the_floor_laughing
   | ":wink:" -> Emoji.winking_face
   | ":cool:" -> Emoji.smiling_face_with_sunglasses
   | ":love_u:" -> Emoji.smiling_face_with_heart_eyes
   | ":thinking:" -> Emoji.thinking_face
   | ":fml:" -> Emoji.expressionless_face
   | ":angry:" -> Emoji.pouting_face
   | ":poop:" -> Emoji.pile_of_poo
   | ":flip_off:" -> Emoji.middle_finger
   | ":high_five:" -> Emoji.raised_hand_with_fingers_splayed
   | ":ok:" -> Emoji.ok_hand
   | ":thumbs_up:" -> Emoji.thumbs_up
   | ":thumbs_down:" -> Emoji.thumbs_down
   | ":fist:" -> Emoji.raised_fist
   | ":bump:" -> Emoji.oncoming_fist
   | ":clap:" -> Emoji.clapping_hands
   | ":jesus:" -> Emoji.folded_hands
   | ":eyes:" -> Emoji.eyes
   | ":tongue:" -> Emoji.tongue
   | ":smooch:" -> Emoji.kiss_mark
   | ":heartbroken:" -> Emoji.broken_heart
   | ":zzz:" -> Emoji.zzz
   | ":bomb:" -> Emoji.bomb
   | ":monkey:" -> Emoji.monkey
   | ":snake:" -> Emoji.snake
   | ":whale:" -> Emoji.spouting_whale
   | ":flower:" -> Emoji.blossom
   | ":eggplant:" -> Emoji.eggplant
   | ":beer:" -> Emoji.beer_mug
   | ":star:" -> Emoji.white_medium_star
   | ":fire:" -> Emoji.fire
   | ":water:" -> Emoji.droplet
   | ":balloon:" -> Emoji.balloon
   | ":b:" -> Emoji.b_button_blood_type_
   | ":smile:" -> Emoji.smiling_face_with_smiling_eyes
   | ":frown:" -> Emoji.slightly_frowning_face
   | ":wow:" -> Emoji.astonished_face
   | _ -> emoji (* Not recognized, so just return it back *)

(* Basically just replaces all valid emoji instaces in [s] with
 * the actual emoji Unicode
 *
 * If no emoji, then just return input string.
 * If emoji is found, return [s] but with regex replaced by actual emoji *)
let rec emoji_replace s acc =
  if (String.length s < 3) then acc^s (* Cant have an emoji in such a short string*)
  else begin
    let regex = Str.regexp ":[A-Za-z0-9_]+:" in (* Regex for emojis *)
      try (
        (* Search from the front and get the index of the first match *)
        let cutoff = Str.search_forward regex s 0 in (* index of first match *)
        let emoji = Str.matched_string s in (* Should be some :asdf: *)
        let offset = String.length emoji + cutoff in
        let rec_substring = Str.string_after s offset in (* everything after the first emoji regex*)
        let needs_replacement = Str.string_before s offset in

        (* Translate the matched emoji into a replacement string
         * This can return the same string back lol *)
        let replacement = get_emoji emoji in
        let replaced = Str.replace_first regex replacement needs_replacement in
        let aug_acc = acc^replaced in
          emoji_replace rec_substring aug_acc
      )
      with
       | _ -> acc^s
  end





