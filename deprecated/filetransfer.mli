(* [target_file] is a type that stores necessary information of a .txt,
 * .jpeg, etc *)
type target_file

(* [convert_to_target file_name] is of type target file, where file_name is
 * the directory and name of a desired file
 * requires: [file_name] is a valid directory and name of a file
 * raises: Not_found *)
val convert_to_target: string -> target_file

(* [convert_to_target file_name tf] will write out a file to location at
 * [file_name], with data from [tf]
 * requires: [file_name] is a valid directory and name of a file
 * raises: Not_found *)
val write_out_target: string -> target_file -> unit