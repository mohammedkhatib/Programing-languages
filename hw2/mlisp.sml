exception Undefined;
exception Empty;


fun initEnv () = fn (str:string) => raise Undefined;


fun define envName oldEnvFun envVal = (fn (s:string) => if (envName = s) then envVal else (oldEnvFun s));

fun emptyNestedEnv () = [initEnv ()];

fun pushEnv newEnv nestedEnvS = newEnv::nestedEnvS;
fun popEnv [] = raise Empty | popEnv nestedEnvS = tl nestedEnvS;
fun topEnv [] = raise Empty | topEnv nestedEnvS = hd nestedEnvS;

fun defineNested envName [] nestedEnvS = raise Empty | defineNested envName nestedEnvS newVal =( define envName (hd nestedEnvS) newVal)::(tl nestedEnvS);

fun find (envName:string) [] = raise Undefined | find (envName:string) nestedEnvS = ((topEnv nestedEnvS ) envName) handle Undefined => find envName (popEnv nestedEnvS);
