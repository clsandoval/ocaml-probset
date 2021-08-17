type expr =
  | Var of string
  | Ld of string * expr
  | Apply of expr * expr
  | IntVal of int
  | IntSucc

type ('e, 'a) state = ('e -> ('a * 'e))
(* 
Transpile Function
*)
let rec transpile expression = 
    match expression with
    |IntSucc -> "lambda x: x + 1"
    |IntVal x ->  string_of_int x
    |Var x ->  x
    |Apply (x,y) -> "("^transpile x^")" ^ "("^transpile y^")"
    |Ld (x,y) -> "lambda "^x^":" ^ transpile y
let zero = Ld ("f", (Ld ("x", Var "x")))
let one = Ld ("f", (Ld ("x", Apply (Var "f", Var "x"))))
let two = Ld ("f", (Ld ("x", Apply (Var "f", Apply (Var "f", Var "x")))))
let plus = Ld ("n", Ld ("f", Ld ("x", Apply ((Var "f"), (Apply ((Var "n"), (Apply (Var "f", Var "x"))))))))
let add = Ld ("m", (Ld ("n", (Ld ("f", (Ld ("x", (Apply ((Apply (Var "m", Var "f")), ((Apply ((Apply (Var "n", Var "f")), Var "x"))))))))))))
let three = Apply(Apply(add,one), two)
let ycomb =  Ld("f", Apply(Ld("x", Apply(Var "f", Apply(Var "x", Var "x"))), Ld("x", Apply(Var "f", Apply(Var "x", Var "x")))))
let tru = Ld("a", (Ld ("b",Var "a")))
let fals = Ld("a", (Ld ("b",Var "b")))
let ifelse = Ld("p", Ld("a", Ld("b", Apply(Apply(Apply(Var "p", Var "a"),Var "b"), Ld("z",Var "x") ))))
let succ = Ld("n", (Ld ("f", Ld("x", Apply( Var "f",Apply( Apply(Var "n", Var "f") , Var "x"))))))
let py1 = (Apply (Apply ((Apply (Apply (add, one), two)), IntSucc), (IntVal 0)))
let iszero = Ld("n", Apply(Apply(Var "n",(Ld("x", fals))), tru))
let zcombinator = Ld("f",Apply(Ld("x", Apply (Var "f", (Ld("z",(Apply(Apply(Var "x",Var "x"),Var "z")))))),Ld("x", Apply(Var "f", Ld("z",(Apply(Apply(Var "x",Var "x"),Var "z")))))))
let pred = Ld("n", Ld("f", Ld ("x", Apply( Apply(Apply (Var "n", Ld("g", Ld("h", Apply(Var "h", Apply(Var "g", Var "f"))))), Ld("u", Var "x")),Ld( "u", Var "u" )))))
let sub = Ld("m", Ld("n", Apply(Apply(Var "n", pred), Var "m")))
(* 
Golomb Term
*)
let previ = Apply(pred, Var "i")
let gnmin =Apply(Var "f", previ)
let oneplus = Apply(add,one)
let golomb =  (Ld("f",Ld("i",Apply(Apply(Apply(ifelse,Apply(iszero,previ)), Ld("t", one)), Ld("t",Apply(oneplus,Apply(Var "f", Apply(Apply(sub,Apply(oneplus,previ)),Apply(Var "f", gnmin)))))))))
let golomb = Apply(zcombinator,golomb)
(* 
Reverse list using Ocaml
*)
let rec revlist acc lis = 
    match lis with
        |[]-> acc
        |head::tail-> revlist (head::acc) tail 
let pair = Ld("a",Ld("b",Ld("c", Apply(Apply(Var "c", Var "a"),Var "b"))))
let fst = Ld("p", Apply(Var "p",Ld("a",Ld("b",Var "a"))))
let snd = Ld("p", Apply(Var "p",Ld("a",Ld("b",Var "b"))))
let isempty = Ld("l",Apply(Apply(Var "l", Ld("h",Ld("t",Ld("d",fals)))),tru))
let ifempty = Ld("q", Apply(ifelse, Apply(isempty, Var "q")))
(* 
Sample list[1; 2; 3; NIL]
*)
let samplist =  Apply(Apply(pair, one),Apply(Apply(pair, two),Apply(Apply(pair, Apply(Apply(add,one),two)),fals)))
let sndb =Apply(snd,Var "b")
let fstb = Apply(fst,Var "b")
(* Function that takes two arguments list and accumulator, returns reversed list*)
let revl2 = (Ld("f",Ld("b",Ld("i", Apply(Apply(Apply(ifempty,Var "b"), Ld("t",Var "i")), Ld("t",Apply(Apply(Var "f", sndb), Apply(Apply(pair, fstb),Var "i"))))))))
let revlis = Apply(zcombinator,revl2)
(*
// TESTING //

GOLOMB TERM
let golomb =transpile golomb
let n = transpile (Apply(Apply(add,(Apply(Apply(add,two),(Apply(Apply(add,two),two))))),(Apply(Apply(add,two),two))))

REVERSE LIST
let c = transpile (Apply(Apply((Apply((fst,Apply(Apply(revlis,samplist),fals)))), IntSucc), IntVal 0))
let c = transpile (Apply(Apply((Apply((fst,Apply( snd, Apply(Apply(revlis,samplist),fals))))), IntSucc), IntVal 0))
let c = transpile (Apply(Apply(Apply(fst,(Apply((snd,Apply( snd, Apply(Apply(revlis,samplist),fals)))))), IntSucc), IntVal 0))

let c = transpile (Apply(Apply(Apply(ifelse,Apply(snd,(Apply((snd,Apply( snd, Apply(Apply(revlis,samplist),fals))))))), Ld("t", two)), Ld("t", one)))

*)