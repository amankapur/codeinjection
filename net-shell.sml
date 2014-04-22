

structure NetShell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator

  val port = 3042

  fun run fenv sock = let
    val (is, os) = SocketIO.openSocket sock
    fun prompt () = (print "func-env-demo> "; SocketIO.inputLine is)
    
    fun pr l = print ((String.concatWith " " l)^"\n")
    fun read fenv =
        (case prompt ()
          of NONE => ()
           | SOME "\f\n" => (Socket.close sock)
           | SOME str => eval_print fenv str)
    and eval_print fenv str =
        (let val ts = P.lexString str
             val _ = pr (["  Tokens ="] @ (map P.stringOfToken ts))
             val expr = P.parse ts
             val _ = pr ["  IR = ", I.stringOfMExpr (expr)]
             val v = (E.shellLoop expr fenv)
             (*val _ = pr [I.stringOfMExpr v]*)
             val _ = pr ["\n"]
         in
           read fenv
         end
         handle P.Parsing msg => (pr ["Parsing error:", msg]; read fenv)
              | E.Evaluation msg => (pr ["Evaluation error:", msg]; read fenv))
  in
    print "Type . by itself to quit\n";
    read (fenv@(E.primitives))    
  end

  fun netshell fenv = Server.mkSingleServer port (fn s => run fenv s)
  

end