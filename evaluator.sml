
structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (*
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq _ _ = evalError "primEq"


  fun stringOfEnvTup (n,v) = String.concat ["|",n, " -> ", I.stringOfMExpr v, "|\n"]

  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) =
        (if (n = name) then
                  v
                else lookup name env)

  (*
   *   Evaluation functions
   *
   *)

  fun isTerminal (I.MExpr _) = false
    | isTerminal (I.MTerm _) = true

  (*fun evalM env (I.MExpr e) = eval env (I.MExpr e)
    | evalM env (I.MTerm t) = (I.MTerm t)*)

  and eval (I.MTerm t, env) = ((I.MTerm t), env)
    | eval (I.MExpr (I.EIf (e,f,g)), env) = if (isTerminal e) then evalIf env e f g else let val (new_exp, new_env) = (eval (e, env)) in ((I.MExpr (I.EIf (new_exp, f, g))), new_env) end
    | eval (I.MExpr (I.ELet (name,e,f)), env) = if (isTerminal e) then evalLet env name e f else let val (new_exp, new_env) = (eval (e, env)) in ((I.MExpr (I.ELet (name, new_exp, f))), new_env) end
    | eval (I.MExpr (I.EIdent n), env) = ((lookup n env), env)
    | eval (I.MExpr (I.ELetFun (name,param,expr,body)), env) = if (isTerminal expr) then evalLetFun env name param expr body else let val (new_exp, new_env) = (eval (expr, env)) in ((I.MExpr (I.ELetFun (name, param, new_exp, body))), new_env) end
    
    | eval (I.MExpr (I.EFun (name, body)), env) = (I.MTerm (I.VClosure (name, body, env)), env)
    | eval (I.MExpr (I.EApp (e1,e2)), env) = (case (isTerminal e2) 
                                                 of true => (case (isTerminal e1)
                                                    of true => (printEnv env "base case" ; (evalApp env e1 e2))
                                                    | false => let val (new_exp, new_env) = (eval (e1, env)) in ((I.MExpr (I.EApp (new_exp, e2))), new_env) end)
                                                 | false => let val (new_exp, new_env) = (eval (e2, env)) in ((I.MExpr (I.EApp (e1, new_exp))), new_env) end)


    | eval (I.MExpr (I.EPrimCall2 (f,e1,e2)), env) = (case e1
                                                         of I.MTerm t1 => (case e2
                                                                      of I.MTerm t2 => (printEnv env "primcall base case"; ((I.MTerm (f t1 t2)), env))
                                                                      | e2 => let val (new_exp, new_env) = (eval (e2, env)) in (I.MExpr 
                                                                        ((I.EPrimCall2 (f, e1
                                                                                      , new_exp))), new_env) end)
                                                         | _ => let val (new_exp, new_env) = (eval (e1, env)) in ((I.MExpr (I.EPrimCall2 (f, new_exp, e2))), new_env) end)

  (*and eval x = eval x*)

  (*and eval env (I.MTerm t) = I.MTerm t    
    | eval env (I.MExpr (I.EIf (e,f,g))) = (I.MExpr (I.EIf (e,f,g)))
    | eval env (I.MExpr (I.ELet (name,e,f))) = (I.MExpr (I.ELet (name,e,f)))
    | eval env (I.MExpr (I.EIdent n)) = lookup n env
    | eval env (I.MExpr (I.ELetFun (name,param,expr,body))) = (I.MExpr (I.ELetFun (name,param,expr,body)))    
    | eval env (I.MExpr (I.EFun (name, body))) = (I.MExpr (I.EFun (name, body)))
    | eval env (I.MExpr (I.EApp (e1,e2))) = (I.MExpr (I.EApp (e1,e2)))
    | eval env (I.MExpr (I.EPrimCall2 (f,e1,e2))) = (I.MExpr (I.EPrimCall2 (f,e1,e2)))*)


  and loop (e, env) _ = (print (String.concat ["e is: ", I.stringOfMExpr e]);
                         (if isTerminal e then e else loop (eval (e, env)) (TextIO.inputLine (TextIO.stdIn))))

  and printEnv env helper = ((print (String.concat ([helper,"\nlookup : \n"]@((List.map stringOfEnvTup env)@["\n"])))); ())
(*  let fun loop e = 
    if terminal (e) then get_value()
    if is_nice_point(e):
      check_changes
      loop(eval e’)
    else loop(eval e)
*)


  and evalApp oldEnv (I.MTerm (I.VClosure (n,body,env))) v = let val new_env = ((n,v)::env) in (let val (new_exp, new_new_env) = (eval (body, new_env)) in (printEnv env "pre eval"; printEnv new_env " new env" ; printEnv new_new_env "new new env"; (new_exp, new_new_env)) end) end
    | evalApp _ (I.MTerm (I.VRecClosure (f,n,body,env))) v = let
          val new_env = [(f,(I.MTerm (I.VRecClosure (f,n,body,env)))),(n,v)]@env
      in
        (let val (new_exp, new_new_env) = (eval (body, new_env)) in (new_exp, new_new_env) end)
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.MTerm (I.VBool true))  f g = let val (new_exp, new_env) = (eval (f, env)) in (new_exp, new_env) end
    | evalIf env (I.MTerm (I.VBool false)) f g = let val (new_exp, new_env) = (eval (g, env)) in (new_exp, new_env) end 
    | evalIf _ _ _ _ = evalError "evalIf"

  and evalLet env id v body = let val new_env = ((id,v)::env) in (let val (new_exp, new_new_env) = (eval (body, new_env)) in (new_exp, new_new_env) end) end

  and evalLetFun env id param expr body = let
      val f = (I.MTerm (I.VRecClosure (id, param, expr, env)))
      val new_env = ((id,f)::env)
      val (new_expr, new_new_env) = (eval (body, new_env))
  in
      (new_expr, new_new_env)
  end


  (*
   *   List of primitives (already in a form suitable for the environment)
   *)

  val primitives =   
      [("+", (I.MTerm (I.VClosure ("a",
                                     (I.MExpr (I.EFun ("b",
                                                       (I.MExpr (I.EPrimCall2 (primPlus,
                                                                               (I.MExpr (I.EIdent "a")),
                                                                               (I.MExpr (I.EIdent "b")))))))),
                                     [])))),
        ("=", (I.MTerm (I.VClosure ("a",
                                     (I.MExpr (I.EFun ("b",
                                                       (I.MExpr (I.EPrimCall2 (primEq,
                                                                               (I.MExpr (I.EIdent "a")),
                                                                               (I.MExpr (I.EIdent "b")))))))),
                                     []))))]


end
