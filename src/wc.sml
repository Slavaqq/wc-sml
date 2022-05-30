(*
Simple word count utility written in Standard ML
*)

datatype file_input =
  File of string
| NoSuchFile of string

datatype output =
  Count of string * (int * int * int)
| Error of string * string

fun get_input () =
  CommandLine.arguments ()

val count_words = List.length o String.tokens Char.isSpace

val count_chars = List.length o String.explode

fun count_file path =
  let
    val file = TextIO.openIn path
    fun count (f, (l, w, c)) =
      case TextIO.inputLine f of
           SOME line => count (f, (l + 1, w + (count_words line), c +
           (count_chars line)))
         | NONE      => (l, w, c)
  in
    count (file, (0, 0, 0)) before TextIO.closeIn file
  end 

fun files_exists paths =
  case paths of
       [] => []
     | p::paths' => if OS.FileSys.access (p, [])
                  then File p :: files_exists paths'
                  else NoSuchFile p :: files_exists paths'

fun count_files paths =
  case paths of
       [] => []
     | File p::paths' => Count (p, count_file p) :: count_files paths' 
     | NoSuchFile p::paths' => Error (p, "No such file or directory") :: count_files
     paths'

fun format_output outputs =
  case outputs of
       [] => ""
    | Count (path, (l, w, c))::outputs' => "\t" ^ (Int.toString l) ^ "\t" ^
    (Int.toString w) ^ "\t" ^ (Int.toString c) ^ " " ^ path ^ "\n" ^
    format_output outputs'
    | Error (path, message) :: outputs' =>  path ^ ": " ^ message ^ "\n" ^
    format_output outputs'

val process = print o format_output o count_files o files_exists

fun main () =
  process (CommandLine.arguments ())

val _ = main ()

