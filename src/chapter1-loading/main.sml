(* test the FFI *)
val message_box = _import "MessageBoxA" : word * string * string * word -> word;

(* val a = message_box (0w0, "hi\000", "ho\000", 0w0)

val () = print (Word32.toString a)
*)

exception NoRoute
exception NoParse

datatype Url = Url of {
    raw : string,
    scheme : substring,
    user : substring option,
    host : substring option,
    port : substring option,
    path : substring,
    query : substring option,
    fragment : substring option
}


fun nonEmpty sub = if Substring.size sub > 0 then SOME sub else NONE
fun parseUrl str =
    let
        val (scheme, rest)= Substring.position "://" (Substring.full str)
        val (authority, rest) =  Substring.position "/" rest
        val (path, rest) = Substring.position "?" rest
        val (path, (query, fragment)) = if Substring.size rest > 0
            then (path, Substring.position "#" rest)
            else let val (p, f) = Substring.position "#" path in (p, (Substring.full "", f)) end
        val (user, host) = let
                val (u, h) = Substring.position "@" authority
            in
                if Substring.size h > 0 then (u, h) else (Substring.full "", u)
            end
        val (host, port) = Substring.position ":" host
    in
        Url {
            raw = str,
            scheme = scheme,
            user = nonEmpty user,
            host = nonEmpty host,
            port = nonEmpty port,
            path = path,
            query = nonEmpty query,
            fragment = nonEmpty fragment
        }
    end

fun dumpUrl (Url x) =
    let
        fun optPrint (SOME str)= print (Substring.string str) |
            optPrint NONE = print "<empty>"
        val _ = print ("scheme: " ^ (Substring.string (#scheme x)))
    in () end

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

val Url x = parseUrl "http://example.org/hello#frag"

val scheme = Substring.string (#scheme x)

val _ = print scheme