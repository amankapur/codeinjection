structure Evaluator = struct

  structure I = InternalRepresentation

  exception Evaluation of string

  fun evalError msg = raise Evaluation msg

  val globalEnv : ((string * ((I.main_expr))) list) ref = ref []

  fun addToGlobal funcName value = ((globalEnv:=(funcName, value)::(!globalEnv)); ())
  
  fun updateGlobal funcName value = ((globalEnv:=(List.foldl (fn ((n,v),y) => if (funcName=n) then (funcName,value)::y else (n,v)::y) [] (!globalEnv))); ())
  
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = (if (n = name) then v else lookup name env)


  fun lookupGlobal (funcName:string) = lookup funcName (!globalEnv)



  (* Diff functions *)
  fun exprEquals (I.MTerm t1) (I.MTerm t2) = (valueEquals t1 t2)
    | exprEquals (I.MExpr (I.EIf (e1, f1, g1), _)) (I.MExpr (I.EIf (e2, f2, g2), _)) =
      (exprEquals e1 e2) andalso (exprEquals f1 f2) andalso (exprEquals g1 g2)
    | exprEquals (I.MExpr (I.EIdent name1, _)) (I.MExpr (I.EIdent name2, _)) = (name1 = name2)
    | exprEquals (I.MExpr (I.ELet (name1, e1, body1), _)) (I.MExpr (I.ELet (name2, e2, body2), _)) =
      (name1 = name2) andalso (exprEquals e1 e2) andalso (exprEquals body1 body2)
    | exprEquals (I.MExpr (I.ELetFun (name1, param1, functionBody1, body1), _)) (I.MExpr (I.ELetFun (name2, param2, functionBody2, body2), _)) =
      (name1 = name2) andalso (param1 = param2) andalso (exprEquals functionBody1 functionBody2) andalso (exprEquals body1 body2)
    | exprEquals (I.MExpr ((I.EApp (e1_old, e2_old)), _)) (I.MExpr ((I.EApp (e1_new, e2_new)), _)) = 
      (exprEquals e1_old e1_new) andalso (exprEquals e2_old e2_new)
    | exprEquals (I.MExpr (I.EFun (name1, body1), _)) (I.MExpr (I.EFun (name2, body2), _)) =
      (name1 = name2) andalso (exprEquals body1 body2)
    | exprEquals _ _ = false

  and valueEquals (I.VInt i1) (I.VInt i2) = (i1 = i2)
    | valueEquals (I.VBool b1) (I.VBool b2) = (b1 = b2)
    | valueEquals (I.VClosure (arg_name1, function_body1, _)) (I.VClosure (arg_name2, function_body2, _)) =
      (arg_name1 = arg_name2) andalso (exprEquals function_body1 function_body2)
    | valueEquals (I.VRecClosure (name1, arg_name1, function_body1, _)) (I.VRecClosure (name2, arg_name2, function_body2, _)) =
      (name1 = name2) andalso (arg_name1 = arg_name2) andalso (exprEquals function_body1 function_body2)
    | valueEquals _ _ = false

  (*
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq _ _ = evalError "primEq"

  fun primPrint a b = ((print (String.concat ["PRINTING: ", I.stringOfValue a])); b)

  
  (*
   *   Evaluation functions
   *
   *)

  fun isTerminal (I.MExpr _) = false
    | isTerminal (I.MTerm _) = true


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


  and shellLoop e env (is,os) = loop (appendToE e env) (is,os)

  and loop e (is,os) = ((SocketIO.output (os, "STEP"); SocketIO.flushOut os);
                     (case (SocketIO.inputLine is)
                     of NONE => ((print "UNKNOWN!\n"); print (String.concat ["e is: ", I.stringOfMExpr e]); (if isTerminal e then e else loop (eval e) (is,os) ))
                      | SOME "\n" => ((print "NO CHANGE!\n"); print (String.concat ["e is: ", I.stringOfMExpr e]); printEnv (!globalEnv) "GLOBAL: "; (if isTerminal e then e else loop (eval e) (is,os) ))
                      | SOME str => ((print "CHANGED!\n"); print (String.concat ["e is: ", I.stringOfMExpr e]); (if isTerminal e then e else loop (eval e) (is,os) ))))


  and printEnv env helper = ((print (String.concat ([helper,"\nlookup : \n"]@((List.map I.stringOfEnvTup env)@["\n"])))); ())




(*  let fun loop e =
    if terminal (e) then get_value()
    if is_nice_point(e):
      check_changes
      loop(eval e’)
    else loop(eval e)
*)

  and addToEnv fName value localEnv = (addToGlobal fName value; (fName, value)::localEnv) 
      
  and evalApp (I.MTerm (I.VClosure (n,body,env))) v = eval (appendToE body ((n,v)::env))
    | evalApp (I.MTerm (I.VRecClosure (funcName, param,body,env))) v = let
      val new_env = addToEnv funcName (I.MTerm (I.VRecClosure (funcName, param, body, env))) ((param, v)::env)
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
      val new_env = addToEnv name f env     
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
                                     [])))),
        ("print", (I.MTerm (I.VClosure ("a",
                                     (I.MExpr ((I.EFun ("b",
                                                       (I.MExpr ((I.EPrimCall2 (primPrint,
                                                                               (I.MExpr ((I.EIdent "a"), [])),
                                                                               (I.MExpr ((I.EIdent "b"), [])))), []))
                                                       )),
                                                [])),
                                     []))))
        ]


end
