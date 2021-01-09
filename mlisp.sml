(* mohammed 319053823 obedat@campus.technion.ac.il     mohammed 206890998 kh.mohammad@campus.technion.ac.il *)
exception MlispError;
exception Undefined;
exception Empty;

datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun initEnv () = fn (str:string) => raise Undefined; 

(*Section 2*)
fun define str f a = (fn (x:string) => if(x = str) then a else (f x));

(*Section 3*)
fun emptyNestedEnv () = [initEnv ()];
fun pushEnv newenv lst = newenv::lst;
fun popEnv [] = raise Empty | popEnv lst = List.tl lst;
fun topEnv [] = raise Empty | topEnv lst = List.hd lst;

(*Section 4*)
fun defineNested _ [] _ = raise Empty | defineNested str lst vlue = (define str (List.hd lst) vlue)::List.tl lst;

(*Section 5*)
fun find str [] = raise Undefined | find (str:string) EnvLst = (topEnv EnvLst str) handle Undefined =>(find str (popEnv EnvLst));




exception MlispError;





fun eval (ATOM NIL) EnvStack = ((ATOM NIL),EnvStack)
  |
    eval (ATOM (NUMBER n)) EnvStack = ((ATOM (NUMBER n)),EnvStack)  
  |
    eval (ATOM (SYMBOL sym)) EnvStack = let
    val x = (find sym EnvStack) handle Undefined => raise MlispError
    in (x, EnvStack) end
  |

  eval (CONS(ATOM(SYMBOL(FuncName)), Actuals)) EnvStack = let
  
  fun AddActualEnv(ATOM(NIL),ATOM(NIL),Env) = Env
    |
        AddActualEnv(ATOM(NIL),formals,Env) = raise MlispError
    |
        AddActualEnv(actuals,ATOM(NIL),Env) = raise MlispError
    |
        AddActualEnv ((CONS(ATOM(SYMBOL(var_name)), tail1),(CONS(var_val,tail2)),old_env))=
     let
       val curr_env=(defineNested var_name old_env var_val);
       in 
         AddActualEnv(tail1,tail2,curr_env)
       end
    
    val CONS(formal_params,func_body) = (find FuncName EnvStack) handle Undefined => raise MlispError;
    val new_stack = AddActualEnv(formal_params, Actuals, EnvStack);
    val (ret_val, garbage_stack) = (eval func_body EnvStack)
    in 
      (ret_val,EnvStack) 
    end  
  |


  eval(CONS(ATOM(SYMBOL("+")),CONS(ATOM(NUMBER(n1)),CONS(ATOM(NUMBER(n2)),ATOM(NIL))))) EnvStack =
            ((ATOM(NUMBER(n1+n2))),EnvStack)
        
  |
  eval(CONS(ATOM(SYMBOL("-")),CONS(ATOM(NUMBER(n1)),CONS(ATOM(NUMBER(n2)),ATOM(NIL))))) EnvStack =
            ((ATOM(NUMBER(n1-n2))),EnvStack)
        
  |
  eval(CONS(ATOM(SYMBOL("*")),CONS(ATOM(NUMBER(n1)),CONS(ATOM(NUMBER(n2)),ATOM(NIL))))) EnvStack =
            ((ATOM(NUMBER(n1*n2))),EnvStack)
  |
  eval(CONS(ATOM(SYMBOL("div")),CONS(ATOM(NUMBER(n1)),CONS(ATOM(NUMBER(n2)),ATOM(NIL))))) EnvStack =
            ((ATOM(NUMBER(n1 div n2))),EnvStack)
  |
   eval (CONS(ATOM(SYMBOL("+")),CONS(sexp1,CONS(sexp2,ATOM(NIL))))) EnvStack =let
     val new_sexp = (CONS(ATOM(SYMBOL("+")),CONS(#1(eval sexp1 EnvStack),CONS(#1(eval sexp2 EnvStack),ATOM(NIL)))));
     in (eval new_sexp EnvStack) end 
  |
   eval (CONS(ATOM(SYMBOL("-")),CONS(sexp1,CONS(sexp2,ATOM(NIL))))) EnvStack =let
     val new_sexp = (CONS(ATOM(SYMBOL("-")),CONS(#1(eval sexp1 EnvStack),CONS(#1(eval sexp2 EnvStack),ATOM(NIL)))));
     in (eval new_sexp EnvStack) end 
  |
   eval (CONS(ATOM(SYMBOL("*")),CONS(sexp1,CONS(sexp2,ATOM(NIL))))) EnvStack =let
     val new_sexp = (CONS(ATOM(SYMBOL("*")),CONS(#1(eval sexp1 EnvStack),CONS(#1(eval sexp2 EnvStack),ATOM(NIL)))));
     in (eval new_sexp EnvStack) end 
  |
     eval (CONS(ATOM(SYMBOL("div")),CONS(sexp1,CONS(sexp2,ATOM(NIL))))) EnvStack =let
     val new_sexp = (CONS(ATOM(SYMBOL("div")),CONS(#1(eval sexp1 EnvStack),CONS(#1(eval sexp2 EnvStack),ATOM(NIL)))));
     in (eval new_sexp EnvStack) end 
  |
     eval (CONS(ATOM(SYMBOL("cons")),CONS(sexp1,CONS(sexp2,ATOM(NIL))))) EnvStack =
       ((CONS(#1(eval sexp1 EnvStack), #1(eval sexp2 EnvStack))), EnvStack)
  |
  eval (CONS(ATOM(SYMBOL("car")),CONS(sexp,ATOM(NIL)))) EnvStack =
        let
        fun cons_head (CONS(sexp1,sexp2)) = sexp1;
        in  ((cons_head(#1(eval sexp EnvStack))), EnvStack)
        end
  |
 eval (CONS(ATOM(SYMBOL("cdr")),CONS(sexp,ATOM(NIL)))) EnvStack =
        let
        fun cons_tail (CONS(sexp1,sexp2)) = sexp2;
        in  ((cons_tail(#1(eval sexp EnvStack))), EnvStack)
        end
  |(*define for variable*)
  eval (CONS(ATOM(SYMBOL("define")),CONS(ATOM(SYMBOL(VarName)),CONS(VarExp,ATOM(NIL))))) EnvStack =
    let
    val EvalVal =  #1(eval VarExp EnvStack);
    val newS = defineNested VarName EnvStack EvalVal;
    in (ATOM(NIL),newS) end
  |(*define for functions*)
  eval (CONS(ATOM(SYMBOL("define")),CONS(ATOM(SYMBOL(Fname)),CONS(params,CONS(Fbody,ATOM(NIL)))))) EnvStack =
     ((ATOM(NIL), defineNested Fname EnvStack (CONS(params,Fbody))))
  |
eval a b = raise MlispError;
