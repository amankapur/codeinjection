structure Evaluator = struct

  structure I = InternalRepresentation

  exception Evaluation of string

  fun evalError msg = raise Evaluation msg

  val globalEnv : ((string * ((I.main_expr))) list) ref = ref []

(* XXX: remdups in irDiff *)
  fun remDups [] = []
    | remDups ((name,ir)::xs) = (name,ir)::(List.filter (fn (compName,_) => compName <> name) xs)

  (*fun addToGlobal funcName value = ((globalEnv:=(funcName, value)::(!globalEnv)); ())*)
  fun addToGlobal funcName value = ((globalEnv:=remDups ((funcName, value)::(!globalEnv))); ())

  (* changeClosureEnv: given old_closure and new_closure, replace the env in new_closure with the
                       env in old_closure. *)
  fun changeClosureEnv (I.MTerm (I.VRecClosure (_, _, _, env))) (I.MTerm (I.VRecClosure (name, param, body, _))) = 
    (I.MTerm (I.VRecClosure (name, param, body, env)))
    | changeClosureEnv _ _ = evalError "changeClosureEnv called on two non-closures"
  
  fun changeGlobal funcName newClosure =
    ((globalEnv :=
      (List.foldl (fn ((n, oldClosure),y) =>
                    if (funcName = n)
                    then (funcName, (changeClosureEnv oldClosure newClosure))::y
                    else (n, oldClosure)::y) [] (!globalEnv))); ())
  fun removeGlobal name =
    ((globalEnv :=
      (List.foldl (fn ((n,v),y) =>
                    if (name = n)
                    then y
                    else (n, v)::y) [] (!globalEnv))); ())

  fun clearGlobal () = ((globalEnv := []); ())

  fun updateGlobals ((name, NONE)::funcList) = (removeGlobal name; updateGlobals funcList)
    | updateGlobals ((name, SOME closure)::funcList) = (changeGlobal name closure; updateGlobals funcList)
    | updateGlobals [] = ()

  fun lookupEnv (name:string) [] = NONE
    | lookupEnv name ((n,v)::env) = (if (n = name) then (SOME v) else lookupEnv name env)


  fun lookup (name:string) env =
    (case (lookupEnv name (!globalEnv))
        of NONE => (case (lookupEnv name env)
                      of NONE => evalError ("lookup failed on "^name)
                    | SOME v => v)
      | SOME v => v)

    (* old new *)

  (* Diff functions *)
  fun irDiff (I.MTerm t1) (I.MTerm t2) currentFunc =
        if (valueEquals t1 t2) then [] else [currentFunc]
    | irDiff (I.MExpr (e1,_)) (I.MExpr (e2,_)) currentFunc = (case (e1, e2) of
        (I.EIf (e1, f1, g1), I.EIf (e2, f2, g2)) =>
          (irDiff e1 e2 currentFunc)@
          (irDiff f1 f2 currentFunc)@
          (irDiff g1 g2 currentFunc)
      | (I.EIdent name1, I.EIdent name2) =>
          if (name1 = name2) then [] else [currentFunc]
      | (I.ELet (name1, e1, body1), I.ELet (name2, e2, body2)) =>
          (if (name1 = name2)
            (* let x = e1 in body1 => let x = e2 in body2
               look at e1, e2 and body1, body2 to find diffs lower down *)
           then (irDiff e1 e2 currentFunc) @ (irDiff body1 body2 currentFunc) 
            (* let x = ... has changed to let y = ... so current func has changed.
               stop looking for diffs lower down.  *)
           else [currentFunc, (name1, NONE)]) 
           (* XXX: Problem with removing funs?*)
      | (I.ELetFun (name1, param1, functionBody1, body1), I.ELetFun (name2, param2, functionBody2, body2)) =>
          (if (name1 = name2)
           then
            let val newFunc = (name2, SOME (I.MTerm (I.VRecClosure (name2, param2, functionBody2, [])))) in
              (irDiff functionBody1 functionBody2 newFunc)
            end
           else [currentFunc,(name1,NONE)])@
          (if (param1 = param2) then [] else [currentFunc])@
          (irDiff body1 body2 currentFunc)
      | (I.EApp (e1_old, e2_old), I.EApp (e1_new, e2_new)) => 
          (irDiff e1_old e1_new currentFunc) @ (irDiff e2_old e2_new currentFunc)
      | (I.EFun (name1, body1), I.EFun (name2, body2)) =>
          (if (name1 = name2) then (irDiff body1 body2 currentFunc) else [currentFunc])
      | (_, _) => [currentFunc])
    | irDiff _ _ currentFunc = [currentFunc]


  and valueEquals (I.VInt i1) (I.VInt i2) = (i1 = i2)
    | valueEquals (I.VBool b1) (I.VBool b2) = (b1 = b2)
(*    | valueEquals (I.VClosure (arg_name1, function_body1, _)) (I.VClosure (arg_name2, function_body2, _)) =
      (arg_name1 = arg_name2) andalso (irDiff function_body1 function_body2)
    | valueEquals (I.VRecClosure (name1, arg_name1, function_body1, _)) (I.VRecClosure (name2, arg_name2, function_body2, _)) =
      (name1 = name2) andalso (arg_name1 = arg_name2) andalso (irDiff function_body1 function_body2)*)
    | valueEquals _ _ = false

  (*
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq _ _ = evalError "primEq"

  fun primPrint a b = ((print (String.concat ["PRINTING: ", I.stringOfValue a, "\n"])); b)

  
  (*
   *   Evaluation functions
   *
   *)

  fun isTerminal (I.MExpr _) = false
    | isTerminal (I.MTerm _) = true


  fun eval (I.MTerm t)  = I.MTerm t
    | eval (I.MExpr e) = (case e
      of (I.EIf (e, f, g), env) =>
        if (isTerminal e)
        then evalIf e f g env
        else I.MExpr ( (I.EIf ((eval (appendToE e env)), f, g)), env)
      | (I.EIdent name, env) => lookup name env
      | (I.ELet (name, e, body), env) =>
        if (isTerminal e)
        then evalLet name e body env
        else I.MExpr (I.ELet (name, eval (appendToE e env), body), env)
      | (I.ELetFun (name, param, functionBody, body), env) => evalLetFun name param functionBody body env
      | ((I.EApp (e1, e2)), env) =>
        if (isTerminal e1)
        then
          (if (isTerminal e2)
          then evalApp e1 e2
          else (I.MExpr (I.EApp (e1, eval (appendToE e2 env)), env)))
        else I.MExpr (I.EApp (eval (appendToE e1 env), e2), env)
      | (I.EPrimCall2 (f, e1, e2), env) =>
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
      | (I.EFun (name, body), env) => I.MTerm (I.VClosure (name, body, env))
    )



  and appendToE (I.MExpr (e, env)) newEnv = (I.MExpr (e, env@newEnv))
    | appendToE (I.MTerm t) _ = (I.MTerm t)


  and printEnv env =
    print (String.concat (["\nCurrent global environment:\n"]@
          (List.map (fn tup => (I.stringOfEnvTup tup)^"\n") env)@
          ["\n"]))

  and printEnvDiff env = 
    print (String.concat (["\nEnvironment DIFF:\n"]@
          (List.map (fn (name, closureOption) => (case closureOption of
            NONE => name^" DELETED\n"
          | SOME closure => (I.stringOfEnvTup (name, closure))^"\n"
          ))  env)@
          ["\n"]))


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
      val new_env = (param, v)::env
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
