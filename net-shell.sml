

structure NetShell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator

(* Utilities *)
  fun pr l = print ((String.concatWith " " l)^"\n")

  fun parse str =
    let val ts = P.lexString str
        val _ = pr (["  Tokens ="] @ (map P.stringOfToken ts))
        val expr = P.parse ts
        val _ = pr ["  IR = ", I.stringOfMExpr (expr)]
    in
      expr
    end
(* Utilities *)

  fun shellLoop e env (is,os) = loop (E.appendToE e env) (E.appendToE e env) (is,os)

  and loop savedIR currentIR (is,os) =
    (SocketIO.output (os, "STEP\n");
     SocketIO.flushOut os;
     (case (SocketIO.inputLine is)
     of NONE => continue savedIR currentIR (is, os)
      | SOME "\n" => continue savedIR currentIR (is, os)
      | SOME str =>
        (let
            val _ = print "\n\n\n\n****************************\n*****    IR CHANGED    *****\n****************************\n\n\n\n"
            val newIR = parse str
            val difference = E.remDups (E.irDiff savedIR newIR ("", NONE)) (* XXX: put remDups in E.irDiff *)
            val _ = E.printEnvDiff difference
            val _ = E.updateGlobals difference
        in
            continue newIR currentIR (is, os)
        end)))

  and continue savedIR currentIR (is, os) =
      (E.printEnv (!E.globalEnv);
      pr ["Current IR:", I.stringOfMExpr currentIR];
      (if E.isTerminal currentIR
      then currentIR
      else loop savedIR (E.eval currentIR) (is,os) ))

  fun run sock = let
    val (is, os) = SocketIO.openSocket sock
    fun readSocket () = (print "reading from socket\n"; SocketIO.inputLine is)

    fun read () =
        (case readSocket ()
          of NONE => ()
           | SOME "\f\n" => (Socket.close sock)
           | SOME str => eval_print str)
    
    and eval_print str =
        (let val expr = parse str
             val _ = (SocketIO.output (os, "READING\n"); SocketIO.flushOut os)
             val v = (shellLoop expr E.primitives (is, os))
             val _ = (SocketIO.output (os, "DONE\n"); SocketIO.flushOut os)
             (*val _ = pr [I.stringOfMExpr v]*)
             val _ = pr ["\n"]
         in
           read ()
         end
         handle P.Parsing msg => (pr ["Parsing error:", msg]; read ())
              | E.Evaluation msg => (pr ["Evaluation error:", msg]; read ()))
  in
    read ()
  end

  fun netshell port = Server.mkSingleServer port (fn s => run s)

end