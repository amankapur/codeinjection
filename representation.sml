
structure InternalRepresentation = struct

  datatype value = VInt of int
                 | VBool of bool
                 | VClosure of string * main_expr * (string * main_expr) list
                 | VRecClosure of string * string * main_expr * (string * main_expr) list

  and expr = EFun of string * main_expr
           | EIf of main_expr * main_expr * main_expr
           | ELet of string * main_expr * main_expr
           | ELetFun of string * string * main_expr * main_expr
           | EIdent of string
           | EApp of main_expr * main_expr
           | EPrimCall2 of (value -> value -> value) * main_expr * main_expr

  and main_expr = MExpr of expr * (string * main_expr) list
                | MTerm of value

  fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strS s = "\""^s^"\""
    fun strV (VInt i) = $ ["VInt ",Int.toString i]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VClosure (n,e,_)) = $ ["VClosure (", n, ",", stringOfMExpr e, ")"]
      | strV (VRecClosure (f,n,e,_)) = $ ["VRecClosure (", f, ",",n, ",", stringOfMExpr e, ")"]
    and strE (EFun (n,e)) = $ ["EFun (", n, ",", stringOfMExpr e, ")"]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" stringOfMExpr [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",stringOfMExpr e1,",",stringOfMExpr e2,")"]
      | strE (ELetFun (n,p,e1,e2)) = $ ["ELetFun (",strS n,",",
                                        strS p, ",",
                                        stringOfMExpr e1,",",stringOfMExpr e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (EApp (e1,e2)) = strCon "EApp" stringOfMExpr [e1,e2]
      | strE (EPrimCall2 (f,e1,e2)) = strCon "EPrimCall2" stringOfMExpr [e1,e2]
    in
      strE e
    end

  and stringOfEnvTup (n,v) = String.concat ["",n, " -> ", stringOfMExpr v, ", "]

  and (*stringOfMExpr (MExpr (e, env)) = String.concat [" MExpr (", stringOfExpr e, " ENV: [", String.concat (List.map stringOfEnvTup env), "] ) "]*)
    stringOfMExpr (MExpr (e, env)) = String.concat [" MExpr (", stringOfExpr e, "^", Int.toString (List.length env), "^", ") "]
    | stringOfMExpr (MTerm t) = String.concat [" {MTerm: ", stringOfValue t, "} "]

  and stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VClosure (n,e,_)) =
        String.concat ["<function (", n, ",", stringOfMExpr e,")>"]
    | stringOfValue (VRecClosure (f,n,e,_)) =
        String.concat ["<function ", f, " (", n, ",", stringOfMExpr e,")>"]

  fun printValue v = (print (stringOfValue v);
                      print "\n")

end
