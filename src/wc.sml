(*
Simple word count utility written in Standard ML
*)

fun get_input () =
  hd (CommandLine.arguments ())

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

fun show (l, w, c) path =
  print ("\t" ^ (Int.toString l) ^ "\t" ^ (Int.toString w) ^ "\t" ^
  (Int.toString c) ^ " " ^ path ^ "\n")

fun main () =
  let 
    val path = get_input ()
  in
    show (count_file path) path
  end

val _ = main ()

