(* test the FFI *)
val message_box = _import "MessageBoxA" : word * string * string * word -> word;

(* val a = message_box (0w0, "hi\000", "ho\000", 0w0)

val () = print (Word32.toString a)
*)

exception NoRoute

fun createConnection hostName =
    let 
        (* Look up the host name *)
        val e = NetHostDB.getByName hostName

        (* Bail out if not existing *)
        val _ = if Option.isSome e then () else raise NoRoute

        (* Get the IP Address *)
        val ip = NetHostDB.addr (valOf e)

        (* Add port for a socket address*)
        val sa = INetSock.toAddr (ip, 80)

        val socket = INetSock.TCP.socket ()

        val _ = Socket.connect (socket, sa)
    in 
    socket
    end

fun write sock str = 
    let 
        val bytes = Byte.stringToBytes str
    in Socket.sendVec (sock, Word8VectorSlice.full bytes) end

fun request host path = 
    let 
        val s = createConnection host

        val _ = write s ("GET " ^ path ^ " HTTP/1.0\r\n")

        val _ = write s ("Host: " ^ host ^ "\r\n\r\n")
        fun readResponse acc = 
            let 
                val v = Socket.recvVec (s, 512)
            in
                if Word8Vector.length v > 0 
                    then readResponse (v::acc)
                    else Byte.bytesToString (Word8Vector.concat (rev acc)) 
            end
        val response = readResponse []
        val _ = Socket.close s
    in
    response
    end

val page = request "example.org" "/"

val _ = print page