(*
Simple word count utility written in Standard ML
*)

datatype file_input =
  File of string
| NoSuchFile of string

datatype output =
  Count of string * (int * int * int)
| Error of string * string

val count_words = List.length o String.tokens Char.isSpace

val count_chars = List.length o String.explode

fun count_file path =
  let
    val stream = TextIO.openIn path
    fun count stream (l, w, c) =
      case TextIO.inputLine stream of
           SOME line => count stream (l + 1, w + (count_words line), c + (count_chars line))
         | NONE      => (l, w, c)
  in
    count stream (0, 0, 0) before TextIO.closeIn stream
  end 

fun files_exists paths =
  let 
    fun aux [] acc = acc
      | aux (path::paths') acc = if OS.FileSys.access (path, [])
                          then aux paths' (File path::acc)
                          else aux paths' (NoSuchFile path::acc)
  in
    aux paths []
  end

fun count_files paths =
  let
    fun aux [] acc = acc
      | aux (File path::paths') acc = aux paths' (Count (path, count_file
      path)::acc)
      | aux (NoSuchFile path::paths') acc = aux paths' (Error (path, "No such file.")::acc)
  in
    aux paths []
  end


fun add_total outputs =
  if List.length outputs > 1
  then 
    let 
      fun get_total [] acc = acc
        | get_total (Count (_, (l, w, c))::outputs') (acc_l, acc_w, acc_c) = get_total outputs' (acc_l + l, acc_w + w, acc_c + c)
        | get_total (_::outputs') acc = get_total outputs' acc 
    in
      outputs @ [Count ("total", get_total outputs (0, 0, 0))]
    end
  else outputs

fun format_output outputs =
  case outputs of
       [] => ""
    | Count (path, (l, w, c))::outputs' => "\t" ^ (Int.toString l) ^ "\t" ^
    (Int.toString w) ^ "\t" ^ (Int.toString c) ^ " " ^ path ^ "\n" ^
    format_output outputs'
    | Error (path, message) :: outputs' =>  path ^ ": " ^ message ^ "\n" ^
    format_output outputs'

val process = print o format_output o add_total o count_files o files_exists

fun main () =
  process (CommandLine.arguments ())

val _ = main ()

