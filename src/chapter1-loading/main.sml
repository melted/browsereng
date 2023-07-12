exception NoRoute
exception NoParse

datatype Url = Url of {
    raw : string,
    scheme : substring,
    user : substring option,
    host : substring,
    port : int option,
    path : substring,
    query : substring option,
    fragment : substring option
}

fun split on str =
    let val (l, r) = Substring.position on str in (l, Substring.triml (size on) r) end

fun splitInOrder splitters str =
    let
        fun worker _ acc NONE = rev acc
            | worker [] acc (SOME rest) = rev (rest::acc)
            | worker remaining acc (SOME slc) =
                let
                    val emptySlice = CharVectorSlice.full (CharVector.fromList []) 
                    fun pred (i, e) = List.exists (fn x => x = e) remaining
                    val (remaining, acc, rest) =
                        case CharVectorSlice.findi pred slc of
                            NONE => ([], (List.map (fn _ => emptySlice) remaining)@slc::acc, NONE)
                            | SOME (idx, elem) =>
                                let
                                    fun go [] _ = raise Empty
                                        | go (x::xs) a = if x = elem then (xs, (CharVectorSlice.subslice (slc, 0, SOME idx))::a, SOME (CharVectorSlice.subslice (slc, idx+1, NONE))) else go xs (emptySlice::a)
                                in go remaining acc end
                in
                    worker remaining acc rest
                end
    in worker splitters [] (SOME str) end

fun nonEmpty sub = if Substring.size sub > 0 then SOME sub else NONE

fun parseUrl str =
    let
        open Substring
        val (scheme, rest)= split "://" (full str)
        val [authority, path, query, fragment] = splitInOrder [#"/", #"?", #"#"] rest
        val [user, host, port] = splitInOrder [#"@", #":"] authority
    in
        Url {
            raw = str,
            scheme = scheme,
            user = if Substring.size host > 0 then nonEmpty user else NONE,
            host = Option.getOpt(nonEmpty host, user),
            port = Option.join (Option.map (Int.fromString o Substring.string) (nonEmpty port)),
            path = Substring.full ("/" ^ Substring.string path),
            query = nonEmpty query,
            fragment = nonEmpty fragment
        }
    end

fun urlScheme (Url u) = Substring.string (#scheme u)
fun urlPath (Url u) = Substring.string (#path u)
fun urlString (Url u) = #raw u
fun urlUser (Url u) = Substring.string (Option.getOpt (#user u, Substring.full ""))
fun urlHost (Url u) = Substring.string (Option.getOpt (#host u, Substring.full ""))
fun urlPort (Url u) = #port u
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
        val _ = print (Option.getOpt (Option.map Int.toString (#port x), "<empty>"))
        val _ = print (", path: " ^ (Substring.string (#path x)))
        val _ = print (", query: ")
        val _ = optPrint (#query x)
        val _ = print (", fragment: ")
        val _ = optPrint (#fragment x)
    in print ")\n" end

fun createConnection hostName port =
    let 
        (* Look up the host name *)
        val e = NetHostDB.getByName hostName

        (* Bail out if not existing *)
        val _ = if Option.isSome e then () else raise NoRoute

        (* Get the IP Address *)
        val ip = NetHostDB.addr (valOf e)

        (* Add port for a socket address*)
        val sa = INetSock.toAddr (ip, port)

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
        val isHttps = urlScheme url = "https"
        val defaultPort = if isHttps then 443 else 80
        val port = Option.getOpt (urlPort url, defaultPort)
        val s = createConnection (urlHost url) defaultPort

        val _ = write s ("GET " ^ (urlPath url) ^ " HTTP/1.0\r\n")

        val _ = write s ("Host: " ^ (urlHost url) ^ "\r\n\r\n")

        val response = readResponse s
        val _ = Socket.close s
    in
        parseResponse response
    end

val x = parseUrl "http://example.org:8000/neat_little_path/?qyuery=/hsbn/#fnuuu"

val _ = dumpUrl x

val _ = print (urlPath x)



val (status, headers, page) = request "http://niklas.org/"

val _ = println status

val n = length headers

val _ = println (Int.toString n)

