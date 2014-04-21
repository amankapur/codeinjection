
structure SocketIO = 

    struct
        
        structure S = Socket
        structure SIO = TextIO.StreamIO
        structure TPIO = TextPrimIO

        open TextIO

        (* opening an (accepted) socket yields two streams
         * constructed from readers and writers to the 
         * opened socket *)
            
        local 
            fun rd (s) = let
		val (haddr, port) = 
		    INetSock.fromAddr (Socket.Ctl.getSockName s)
		val sockName = String.concat [NetHostDB.toString haddr,
					      ":",
					      Int.toString port]
	    in
		TPIO.RD {name = sockName,
                         chunkSize = Socket.Ctl.getRCVBUF s,
                         readVec = SOME(fn sz => Byte.bytesToString(S.recvVec (s,sz))),
                         readArr = NONE,
                         readVecNB = NONE,
                         readArrNB = NONE,
                         block = NONE,
                         canInput = NONE,
                         avail = fn () => NONE,
                         getPos = NONE,
                         setPos = NONE,
                         endPos = NONE,
                         verifyPos = NONE,
                         close = fn () => Socket.close s,
                         ioDesc = NONE}
	    end

            fun wr (s) = let
		val (haddr, port) = INetSock.fromAddr (Socket.Ctl.getSockName s)
		val sockName = String.concat [NetHostDB.toString haddr,
					      ":",
					      Int.toString port]
		fun writeVec buffer = let
		    val (str,i,sz) = CharVectorSlice.base buffer
		    val slice = 
			Word8VectorSlice.slice (Byte.stringToBytes(str),i,SOME sz)
		in
		    S.sendVec (s,slice)
		end 
	    in
		TPIO.WR {name = sockName,
                         chunkSize = Socket.Ctl.getSNDBUF s,
                         writeVec = SOME (writeVec),
                         writeArr = NONE,
                         writeVecNB = NONE,
                         writeArrNB = NONE,
                         block = NONE,
                         canOutput = NONE,
                         getPos = NONE,
                         setPos = NONE,
                         endPos = NONE,
                         verifyPos = NONE,
                         close = fn () => Socket.close s,
                         ioDesc = NONE}
	    end
        in
            
            fun openSocket (s) = 
                let val inS = StreamIO.mkInstream (rd(s),"")
                    val outS = StreamIO.mkOutstream (wr(s),IO.NO_BUF)
                in
                    (mkInstream (inS),
                     mkOutstream (outS))
                end
        end
    end
