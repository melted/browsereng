
val message_box = _import "MessageBoxA" : word * string * string * word -> word;

val a = message_box (0w0, "hi\000", "ho\000", 0w0)

val () = print (Word32.toString a)

