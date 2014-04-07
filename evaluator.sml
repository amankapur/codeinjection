
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


  (*
   *   Substitution function
   *)

  fun subst (I.EVal (I.VFun (n,body))) id e = 
        if id = n then 
          I.EVal (I.VFun (n,body))
        else I.EVal (I.VFun (n, subst body id e))
    | subst (I.EVal v) id e = I.EVal v
    | subst (I.EIf (e1,e2,h)) id e = 
        I.EIf (subst e1 id e, subst e2 id e, subst h id e)
    | subst (I.ELet (id',e1,e2)) id e = 
        if id = id' then 
	  I.ELet (id',subst e1 id e, e2)
	else I.ELet (id',subst e1 id e, subst e2 id e)
    | subst (I.ELetFun (id',param,e1,e2)) id e = 
        if id = id' then 
	  I.ELetFun (id',param, e1, e2)
	else if id = param then
	  I.ELetFun (id',param, e1, subst e2 id e)
	else
	  I.ELetFun (id',param, subst e1 id e, subst e2 id e)
    | subst (I.EIdent id') id e = 
        if id = id' then e else I.EIdent id'
    | subst (I.EApp (e1,e2)) id e = I.EApp (subst e1 id e, subst e2 id e)
    | subst (I.EPrimCall2 (f,e1,e2)) id e = 
        I.EPrimCall2 (f, subst e1 id e, subst e2 id e)
			       
			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = 
        if (n = name) then 
	  v
	else lookup name env 


  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
      
  and evalApp env (I.VFun (n,body)) v = eval env (subst body n (I.EVal v))
    | evalApp env _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"
		       
  and evalLet env id v body = eval env (subst body id (I.EVal v))


  (* uses a fixed-point combinator *)

  and evalLetFun env id param expr body = let
      val t1 = I.EVal (I.VFun ("v", I.EApp (I.EApp (I.EIdent "x",
						    I.EIdent "x"),
					    I.EIdent "v")))
      val t2 = I.EVal (I.VFun ("x", I.EApp (I.EIdent "f",t1)))
      val Z = I.VFun ("f", I.EApp (t2,t2))
      val F = I.VFun (id, I.EVal (I.VFun (param, expr)))
      val f = I.EApp (I.EVal Z, I.EVal F)
  in
      eval env (subst body id f)
  end


  (* 
   *   List of primitives (already in a form suitable for the environment)
   *)

  val primitives =
      [("+", I.VFun ("a", 
		     I.EVal (I.VFun ("b", 
				     I.EPrimCall2 (primPlus,
						   I.EIdent "a",
						   I.EIdent "b"))))),
       ("=", I.VFun ("a",
		     I.EVal (I.VFun ("b",
				     I.EPrimCall2 (primEq,
						   I.EIdent "a",
						   I.EIdent "b")))))]
  
				 
end
