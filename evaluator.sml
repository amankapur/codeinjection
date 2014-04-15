
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

(*  and eval (I.MTerm t, env) = ((I.MTerm t), env)
    | eval (I.MExpr (I.EIf (e,f,g)), env) = if (isTerminal e) then evalIf env e f g else let val (new_exp, new_env) = (eval (e, env)) in ((I.MExpr (I.EIf (new_exp, f, g))), new_env) end
    | eval (I.MExpr (I.ELet (name,e,f)), env) = if (isTerminal e) then evalLet env name e f else let val (new_exp, new_env) = (eval (e, env)) in ((I.MExpr (I.ELet (name, new_exp, f))), new_env) end
    | eval (I.MExpr (I.EIdent n), env) = ((lookup n env), env)
    | eval (I.MExpr (I.ELetFun (name,param,expr,body)), env) = if (isTerminal expr) then evalLetFun env name param expr body else let val (new_exp, new_env) = (eval (expr, env)) in ((I.MExpr (I.ELetFun (name, param, new_exp, body))), new_env) end
    
    | eval (I.MExpr (I.EFun (name, body)), env) = (I.MTerm (I.VClosure (name, body, env)), env)
    | eval (I.MExpr (I.EApp (e1,e2)), env) = (case (isTerminal e2) 
                                                 of true => (case (isTerminal e1)
                                                    of true => (printEnv env "base case" ; (evalApp env e1 e2))
                                                    | false => let val (new_exp, new_env) = (eval (e1, env)) in ((I.MExpr (I.EApp (new_exp, e2))), env) end)
                                                 | false => let val (new_exp, new_env) = (eval (e2, env)) in ((I.MExpr (I.EApp (e1, new_exp))), env) end)


    | eval (I.MExpr (I.EPrimCall2 (f,e1,e2)), env) = (case e1
                                                         of I.MTerm t1 => 
                                                          (case e2
                                                            of I.MTerm t2 => (printEnv env "primcall base case"; ((I.MTerm (f t1 t2)), env))
                                                            | e2 => let val (new_exp, new_env) = (eval (e2, env)) in (I.MExpr 
                                                              ((I.EPrimCall2 (f, e1, new_exp))), new_env) end)
                                                         | _ => let val (new_exp, new_env) = (eval (e1, env)) in ((I.MExpr (I.EPrimCall2 (f, new_exp, e2))), new_env) end)
*)
  
  fun eval (I.MTerm t)  = I.MTerm t
    | eval (I.MExpr (I.EIf (e, f, g), env)) =
      if (isTerminal e)
      then evalIf e f g env 
      else I.MExpr ( (I.EIf ((eval (appendToE e env)), f, g)), env)
    | eval (I.MExpr (I.EIdent name, env)) = lookup name env
    | eval (I.MExpr (I.ELet (name, e, body), env)) =
      if (isTerminal e)
      then evalLet name e body env
      else I.MExpr (I.ELet (name, eval (appendToE e env), body), env)
    | eval (I.MExpr (I.ELetFun (name, param, functionBody, body), env)) = evalLetFun name param functionBody body env
    | eval (I.MExpr ((I.EApp (e1, e2)), env)) = 
      if (isTerminal e1) 
      then
        (if (isTerminal e2)
        then evalApp e1 e2
        else (I.MExpr (I.EApp (e1, eval (appendToE e2 env)), env)))
      else I.MExpr (I.EApp (eval (appendToE e1 env), e2), env)
    | eval (I.MExpr (I.EPrimCall2 (f, e1, e2), env)) =
      if (isTerminal e1) 
      then
        (if (isTerminal e2)
        then (let val I.MTerm t1 = e1
                  val I.MTerm t2 = e2
              in
                I.MTerm (f t1 t2)
              end)
        else (I.MExpr (I.EPrimCall2 (f, e1, eval (appendToE e2 env)), env)))
      else (I.MExpr (I.EPrimCall2 (f, eval (appendToE e1 env), e2), env))
    | eval (I.MExpr (I.EFun (name, body), env)) = I.MTerm (I.VClosure (name, body, env))


  and appendToE (I.MExpr (e, env)) newEnv = (I.MExpr (e, env@newEnv))
    | appendToE (I.MTerm t) _ = (I.MTerm t)
    


  (*and eval _ = evalError "ERROr: Not implemented"*)

  (*and eval x = eval x*)

  (*and eval env (I.MTerm t) = I.MTerm t    
    | eval env (I.MExpr (I.EIf (e,f,g))) = (I.MExpr (I.EIf (e,f,g)))
    | eval env (I.MExpr (I.ELet (name,e,f))) = (I.MExpr (I.ELet (name,e,f)))
    | eval env (I.MExpr (I.EIdent n)) = lookup n env
    | eval env (I.MExpr (I.ELetFun (name,param,expr,body))) = (I.MExpr (I.ELetFun (name,param,expr,body)))    
    | eval env (I.MExpr (I.EFun (name, body))) = (I.MExpr (I.EFun (name, body)))
    | eval env (I.MExpr (I.EApp (e1,e2))) = (I.MExpr (I.EApp (e1,e2)))
    | eval env (I.MExpr (I.EPrimCall2 (f,e1,e2))) = (I.MExpr (I.EPrimCall2 (f,e1,e2)))*)

  and shellLoop e env = loop (appendToE e env) NONE    

  and loop e _ = (print (String.concat ["e is: ", I.stringOfMExpr e]);
                         (if isTerminal e then e else loop (eval e) (TextIO.inputLine (TextIO.stdIn))))

  (*and printEnv env helper = ((print (String.concat ([helper,"\nlookup : \n"]@((List.map stringOfEnvTup env)@["\n"])))); ())*)
(*  let fun loop e = 
    if terminal (e) then get_value()
    if is_nice_point(e):
      check_changes
      loop(eval e’)
    else loop(eval e)
*)


  (*and evalApp oldEnv (I.MTerm (I.VClosure (n,body,env))) v = let val new_env = ((n,v)::env) in (let val (new_exp, new_new_env) = (eval (body, new_env)) in (printEnv env "pre eval"; printEnv new_env " new env" ; printEnv new_new_env "new new env"; (new_exp, oldEnv)) end) end
    | evalApp _ (I.MTerm (I.VRecClosure (f,n,body,env))) v = let
          val new_env = [(f,(I.MTerm (I.VRecClosure (f,n,body,env)))),(n,v)]@env
      in
        (let val (new_exp, new_new_env) = (eval (body, new_env)) in (new_exp, new_new_env) end)
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"
*)

  and evalApp (I.MTerm (I.VClosure (n,body,env))) v = eval (appendToE body ((n,v)::env))
    | evalApp (I.MTerm (I.VRecClosure (funcName,n,body,env))) v = let
      val new_env = [(funcName, (I.MTerm (I.VRecClosure (funcName,n,body,env)))),(n,v)]@env
      in 
        eval (appendToE body new_env)
      end
    | evalApp _ _ = evalError "cannot apply non-functional value"



  and evalIf (I.MTerm (I.VBool true))  f g env = eval (appendToE f env)
    | evalIf (I.MTerm (I.VBool false)) f g env = eval (appendToE g env)
    | evalIf _ _ _ _ = evalError "evalIf"

  and evalLet name termV body env = let val new_env = ((name, termV)::env) in eval (appendToE body new_env) end
  
  and evalLetFun name param functionBody body env = let
      val f = (I.MTerm (I.VRecClosure (name, param, functionBody, env)))
      val new_env = ((name,f)::env)      
  in
      appendToE body new_env
  end


  (*
   *   List of primitives (already in a form suitable for the environment)
   *)

  val primitives =   
      [("+", (I.MTerm (I.VClosure ("a",
                                     (I.MExpr ((I.EFun ("b",
                                                       (I.MExpr ((I.EPrimCall2 (primPlus,
                                                                               (I.MExpr ((I.EIdent "a"), [])),
                                                                               (I.MExpr ((I.EIdent "b"), [])))), []))
                                                       )),
                                               [])),
                                     [])))),
        ("=", (I.MTerm (I.VClosure ("a",
                                     (I.MExpr ((I.EFun ("b",
                                                       (I.MExpr ((I.EPrimCall2 (primEq,
                                                                               (I.MExpr ((I.EIdent "a"), [])),
                                                                               (I.MExpr ((I.EIdent "b"), [])))), []))
                                                       )),
                                                [])),
                                     []))))]


end
