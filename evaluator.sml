
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
        if (n = name) then
          v
        else lookup name env


  (*
   *   Evaluation functions
   *
   *)

  fun isTerminal (I.MExpr _) = false
    | isTerminal (I.MTerm _) = true

  fun evalM env (I.MExpr e) = eval env (I.MExpr e)
    | evalM env (I.MTerm t) = (I.MTerm t)

  and eval env (I.MTerm t) = I.MTerm t    
    | eval env (I.MExpr (I.EIf (e,f,g))) = if (isTerminal e) then evalIf env e f g else (I.MExpr (I.EIf ((eval env e), f, g)))        
    | eval env (I.MExpr (I.ELet (name,e,f))) = if (isTerminal e) then evalLet env name e f else (I.MExpr (I.ELet (name, (eval env e), f)))
    | eval env (I.MExpr (I.EIdent n)) = lookup n env
    | eval env (I.MExpr (I.ELetFun (name,param,expr,body))) = if (isTerminal expr) then evalLetFun env name param expr body else (I.MExpr (I.ELetFun (name, param, (eval env expr), body)))
    
    | eval env (I.MExpr (I.EFun (name, body))) = if (isTerminal body) then (I.MTerm (I.VClosure (name, body, env))) else (I.MExpr (I.EFun (name, (eval env body))))
    | eval env (I.MExpr (I.EApp (e1,e2))) = (case (isTerminal e1) 
                                                 of true => (case (isTerminal e2)
                                                    of true => (evalApp env e1 e2)
                                                    | false => (I.MExpr (I.EApp (e1, (eval env e2)))))
                                                 | false => (I.MExpr (I.EApp ((eval env e1), e2))))


    | eval env (I.MExpr (I.EPrimCall2 (f,e1,e2))) = (case e1
                                                         of I.MTerm t1 => (case e2
                                                                      of I.MTerm t2 => (I.MTerm (f t1 t2))
                                                                      | e2 => (I.MExpr 
                                                                        (I.EPrimCall2 (f, e1
                                                                                      , (eval env e2)))))
                                                         | e2 => (I.MExpr (I.EPrimCall2 (f, (eval env e1), e2))))    


(*START*)
(*| eval (elet (n, e1, e2) = if (isTerminal e1) then subst e2 n e1 else eLet (n, eval e1, e2)

| eval Eadd (e1, e2) = 
  if (isTerminal e1) then if (isTerminal e2) Eadd (e1, e2) else Eadd (e1, eval e2) else Eadd (eval e1, eval e2)


Bs. Call eval. 

SS.
  let fun loop e = 
    if terminal (e) then get_value()
    if is_nice_point(e):
      check_changes
      loop(eval e’)
    else loop(eval e)*)
(*STOP*)


(*  THE OLD EVAL
  fun eval (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)*)



  and evalApp _ (I.MTerm (I.VClosure (n,body,env))) v = eval ((n,v)::env) body
    | evalApp _ (I.MTerm (I.VRecClosure (f,n,body,env))) v = let
          val new_env = [(f,(I.MTerm (I.VRecClosure (f,n,body,env)))),(n,v)]@env
      in
          eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.MTerm (I.VBool true)) f g = eval env f
    | evalIf env (I.MTerm (I.VBool false)) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"

  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id param expr body = let
      val f = (I.MTerm (I.VRecClosure (id, param, expr, env)))
  in
      eval ((id,f)::env) body
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
