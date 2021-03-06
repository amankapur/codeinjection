(* Default Shell to run the socket based interpreter *)

structure NetShell = struct

  structure P = Parser
  structure I = InternalRepresentation
  structure E = Evaluator

(* Utilities *)
  fun pr l = print ((String.concatWith " " l)^"\n")

  fun parse str =
    let val ts = P.lexString str
        val expr = P.parse ts
    in
      expr
    end
(* Utilities *)

  fun shellLoop expr (is,os) =
    let val exprWithPrimitives = (E.setLocalEnv expr E.primitives) 
    in
      loop exprWithPrimitives exprWithPrimitives (is,os)
    end

  (* Main loop to the find out the diff between IRs *)
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
      ( (*pr [I.stringOfMExpr currentIR, "\n\n"];*)
        if E.isTerminal currentIR
            then currentIR
            else loop savedIR (E.eval currentIR) (is,os))


  (* Opens the socket, reads from socket, and then evals the input*)
  fun run sock = let
    val (is, os) = SocketIO.openSocket sock
    fun readSocket () = SocketIO.inputLine is

    fun read () =
        (case readSocket ()
          of NONE => ()
           | SOME "\f\n" => (Socket.close sock)
           | SOME "\n" => read ()
           | SOME str => eval_print str)
    
    and eval_print str =
        (let val expr = parse str
             val _ = (SocketIO.output (os, "READING\n"); SocketIO.flushOut os)
             val _ = E.clearGlobal ()
             val v = (shellLoop expr (is, os))
             val _ = (SocketIO.output (os, "DONE\n"); SocketIO.flushOut os)
             val _ = pr [I.stringOfMExpr v, "\n"]
         in
           read ()
         end
         handle P.Parsing "" => read ()
              | P.Parsing msg => (pr ["Parsing error:", msg, ": ", str]; read ())
              | E.Evaluation msg => (pr ["Evaluation error:", msg]; read ()))
  in
    read ()
  end

  fun netshell port = Server.mkSingleServer port (fn s => run s)

end