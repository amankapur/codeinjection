

structure NetShell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator

  val port = 3037

  fun stripNewline str =
    if (size str) < 2
    then str
    else (substring (str,0,(size str)-2))

  fun run fenv sock = let
    val (is, os) = SocketIO.openSocket sock
    fun prompt () = (print "func-env-demo> "; SocketIO.inputLine is)
    
    fun pr l = print ((String.concatWith " " l)^"\r\n")
    fun read fenv =
        (case prompt ()
          of NONE => ()
           | SOME ".\r\n" => (Socket.close sock)
           | SOME str => eval_print fenv (stripNewline str))
    and eval_print fenv str =
        (let val ts = P.lexString str
             val _ = pr (["  Tokens ="] @ (map P.stringOfToken ts))
             val expr = P.parse ts
             val _ = pr ["  IR = ", I.stringOfMExpr (expr)]
             val v = (E.shellLoop expr fenv)
             (*val _ = pr [I.stringOfMExpr v]*)
             val _ = pr ["\r\n"]
         in
           read fenv
         end
         handle P.Parsing msg => (pr ["Parsing error:", msg]; read fenv)
              | E.Evaluation msg => (pr ["Evaluation error:", msg]; read fenv))
  in
    print "Type . by itself to quit\r\n";
    read (fenv@(E.primitives))    
  end

  fun netshell fenv = Server.mkSingleServer port (fn s => run fenv s)
  

end


