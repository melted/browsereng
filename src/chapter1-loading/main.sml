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

fun split on str =
    let val (l, r) = Substring.position on str in (l, Substring.triml (size on) r) end

fun nonEmpty sub = if Substring.size sub > 0 then SOME sub else NONE
fun parseUrl str =
    let
        open Substring
        val (scheme, rest)= split "://" (full str)
        val (authority, rest) =  position "/" rest
        val rest = if size rest = 0 then full "/" else rest
        val (path, rest) = split "?" rest
        val (path, (query, fragment)) = if Substring.size rest > 0
            then (path, split "#" rest)
            else let val (p, f) = split "#" path in (p, (full "", f)) end
        val (user, host) = let
                val (u, h) = split "@" authority
            in
                if Substring.size h > 0 then (u, h) else (Substring.full "", u)
            end
        val (host, port) = split ":" host
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

fun urlScheme (Url u) = Substring.string (#scheme u)
fun urlPath (Url u) = Substring.string (#path u)
fun urlString (Url u) = #raw u
fun urlUser (Url u) = Substring.string (Option.getOpt (#user u, Substring.full ""))
fun urlHost (Url u) = Substring.string (Option.getOpt (#host u, Substring.full ""))
fun urlPort (Url u) = Substring.string (Option.getOpt (#port u, Substring.full ""))
fun urlQuery (Url u) = Substring.string (Option.getOpt (#query u, Substring.full ""))
fun urlFragment (Url u) = Substring.string (Option.getOpt (#fragment u, Substring.full ""))

fun println p = print (p ^ "\n")

fun dumpUrl (Url x) =
    let
        fun optPrint (SOME str)= print (Substring.string str) |
            optPrint NONE = print "<empty>"
        val _ = print ("Url(scheme: " ^ (Substring.string (#scheme x)))
        val _ = print (", user: ")
        val _ = optPrint (#user x)
        val _ = print (", host: ")
        val _ = optPrint (#host x)
        val _ = print (", port: ")
        val _ = optPrint (#port x)
        val _ = print (", path: " ^ (Substring.string (#path x)))
        val _ = print (", query: ")
        val _ = optPrint (#query x)
        val _ = print (", fragment: ")
        val _ = optPrint (#fragment x)
    in print ")\n" end

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

fun readResponse socket = 
    let 
        fun worker acc =  
            let 
                val v = Socket.recvVec (socket, 512)
            in
                if Word8Vector.length v > 0 
                    then worker (v::acc)
                    else Word8Vector.concat (rev acc) 
            end
    in
        worker []
    end

fun splitLine slc =
    let
        open Word8VectorSlice
        fun pred (i, e) = e = 0w13 andalso sub (slc, i+1) = 0w10
        val res = case findi pred slc of
                            NONE => (subslice (slc, 0, NONE), subslice (slc, length slc-1, NONE))
                          | SOME (idx, _) => (subslice (slc, 0, SOME idx), subslice (slc, idx+2, NONE)) 
    in res end

fun getHeaders slc = 
    let
        open Word8VectorSlice
        fun worker bytes acc =
            let val (next, rest) = splitLine bytes
            in if length next > 0 
                then worker rest ((split ":" (Substring.full (Byte.unpackStringVec next)))::acc)
                else (rev acc, rest)
            end
    in
        worker slc []
    end

fun parseResponse resp =
    let
        val (statusLine, rest) = splitLine (Word8VectorSlice.full resp)
        val (headers, body) = getHeaders rest
    in (Byte.unpackStringVec statusLine, headers, body) end

fun request urlStr = 
    let 
        val url = parseUrl urlStr
        val s = createConnection (urlHost url)

        val _ = write s ("GET " ^ (urlPath url) ^ " HTTP/1.0\r\n")

        val _ = write s ("Host: " ^ (urlHost url) ^ "\r\n\r\n")

        val response = readResponse s
        val parsed = parseResponse response
        val _ = Socket.close s
    in
    parsed
    end

val (status, headers, page) = request "http://example.org/"

val _ = println status

val n = length headers

val _ = println (Int.toString n)

val x = parseUrl "http://example.org:8080"

val _ = dumpUrl x

val z = Byte.stringToBytes "Hello\r\n, hello"
val (x, y) = splitLine (Word8VectorSlice.full z)
val _ = println (Byte.unpackStringVec x)