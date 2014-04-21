
structure Server = struct

  val traceFlag = ref true
		  
  fun trace s = if (!traceFlag) then 
		  print (concat ["*** ",s," ***\n"])
                else ()


  (*   Create a server on a given port of the current machine
   *   Function f is a callback, called with the created socket 
   *)
                       
  fun mkSingleServer port f = let
      val mySock = INetSock.TCP.socket ()
      fun loop () = let
          val (inSock, addr) = Socket.accept mySock before trace "CONNECTION"
      in
          f inSock
      end
  in
      trace "BINDING PORT";
      Socket.bind (mySock, INetSock.any port);
      trace "LISTENING TO PORT";
      Socket.listen (mySock, 1);
      loop ();
      Socket.close mySock;
      ()
  end

end
