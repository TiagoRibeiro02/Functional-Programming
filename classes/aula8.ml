(* modules *)

module type MATHLIB = sig
  val fact : int -> int

  val half_pi : float

  (*val doubler : float -> float*)
end

(* se o MATHLIB não estiver a "esconder nada", podemos deixá-lo
   desligado e o OCaml fará a mesma coisa, 
   mas com o doubler comentado acima, ele esconde
   coisas de fora do módulo. *)
module MyMathLib : MATHLIB = struct
  let rec fact x =
    if x = 0 then
      1
    else
      x * fact (x - 1)

  let half_pi = Float.pi /. 2.0

  let doubler x = x *. 2.0

  let pi = doubler half_pi
end

let pi = MyMathLib.half_pi +. MyMathLib.half_pi

(* simplesmente uma variável indefinida se o doubler não estiver 
   no tipo de módulo*)
(*let twenty_eight = MyMathLib.doubler 14.0 *)

(* ---------------------------------------- *)

(* esta assinatura esconde o gcd e o reduce (e o whole).  Desta forma 
  os utilizadores não podem assumir que eles existem e não podem chamá-los com
  entradas inesperadas. *)
module type RATIONAL_A = sig
  type rational =
    | Whole of int
    | Frac of int * int

  exception BadFrac

  (* o tuplo parece mais uma fração, talvez*)
  val make_frac : int * int -> rational

  val add : rational -> rational -> rational

  val string_of_rational : rational -> string
end

(* a assinatura anterior permite que os utilizadores construam 
 qualquer valor do tipo racional que
 que queiram, expondo os construtores Frac e Whole.
 Isto torna impossível manter invariantes 
 sobre racionais, pelo que podemos ter denominadores negativos,
 que algumas funções não tratam, 
 e print_rat pode imprimir uma fração não reduzida.  
 Resolvemos isto tornando os racionais abstractos. *)
module type RATIONAL_B = sig
  type rational (* o tipo é agora abstrato *)

  exception BadFrac

  val make_frac : int * int -> rational

  val add : rational -> rational -> rational

  val string_of_rational : rational -> string
end

(* acontece que, embora o RATIONAL_B seja totalmente poderoso, ele
   não "quebra nada" ao expor toda a função.
*)
module type RATIONAL_C = sig
  type rational

  exception BadFrac

  val whole : int -> rational (* pode expor Whole através da função  *)

  val make_frac : int * int -> rational

  val add : rational -> rational -> rational

  val string_of_rational : rational -> string
end

(* ---------------------------------------- *)

(* pode atribuir qualquer um dos três tipos de módulos acima referidos *)
module Rational1 : RATIONAL_C = struct
  (*
      Invariante 1: all denominators > 0
      Invariante 2: os racionais são mantidos na forma reduzida
  *)
  type rational =
    | Whole of int
    | Frac of int * int

  exception BadFrac

  let rec gcd x y =
    if x = y then
      x
    else if x > y then
      gcd y x
    else
      gcd x (y - x)

  let reduce r =
    match r with
    | Whole _ -> r
    | Frac (0, _) -> Whole 0
    | Frac (n, d) ->
        let g = gcd (abs n) d in
        if g = d then
          Whole (n / d)
        else
          Frac (n / g, d / g)

  (* ao fazer uma fração, banimos os denominadores zero e
    reduzimos a fração
  *)
  let rec make_frac (n,d) =
    if d = 0 then
      raise BadFrac
    else if d < 0 then
      reduce (Frac (-n, -d))
    else
      reduce (Frac (n, d))

  let whole x = Whole x

  (* usando propriedades matemáticas, ambos os invariantes são válidos para o resultado
   assumindo que são válidos para os argumentos *)
  let rec add r1 r2 =
    match r1, r2 with
    | Whole i1,    Whole i2      -> Whole (i1 + i2)
    | Whole i1,    Frac (n2, d2) -> Frac ((i1 * d2) + n2, d2)
    | Frac _,      Whole _       -> add r2 r1
    | Frac(n1,d1), Frac (n2, d2) -> reduce (Frac ((n1 * d2) + (n2 * d1), d1 * d2))

  (* dado o invariante, imprime em forma reduzida *)
  let string_of_rational r =
    match r with
    | Whole i -> string_of_int i
    | Frac (n, d) -> string_of_int n ^ "/" ^ string_of_int d
end

(* pode atribuir qualquer uma das três assinaturas acima *)
(* e **se** usarmos B ou C, este módulo é *equivalente* a Racional1 *)
(* optamos por não reduzir as fracções até à impressão *)
module Rational2 : RATIONAL_B = struct
  type rational =
    | Whole of int
    | Frac of int * int

  exception BadFrac

  let whole n = Whole n

  let rec make_frac (n, d) =
    if d = 0 then
      raise BadFrac
    else if d < 0 then
      Frac (-n, -d)
    else
      Frac (n, d)

  (* nenhuma chamada para reduce! *)
  let rec add r1 r2 =
    match (r1, r2) with
    | Whole i1,     Whole i2     -> Whole (i1 + i2)
    | Whole i1,     Frac (n2,d2) -> Frac ((i1 * d2) + n2, d2)
    | Frac _,       Whole _      -> add r2 r1
    | Frac (n1,d1), Frac (n2,d2) -> Frac ((n1 * d2) + (n2 * d1), d1 * d2)

  let string_of_rational r =
    let rec gcd x y =
      if x = y then
        x
      else if x > y then
        gcd y x
      else
        gcd x (y - x)
    in
    let reduce r =
      match r with
      | Whole _ -> r
      | Frac (0, _) -> Whole 0
      | Frac (n, d) ->
          let g = gcd (abs n) d in
          if g = d then
            Whole (n / d)
          else
            Frac (n / g, d / g)
    in
    match reduce r with
    | Whole i -> string_of_int i
    | Frac (n, d) -> string_of_int n ^ "/" ^ string_of_int d
end

(* esta estrutura utiliza um tipo abstrato diferente.  
   Ela não tem a assinatura RATIONAL_A.  
   Para RATIONAL_C, a função whole está disponível num tipo menos
   geral externamente.
*)
module Rational3 : RATIONAL_B = struct
  type rational = int * int

  exception BadFrac

  let rec make_frac (x, y) =
    if y = 0 then
      raise BadFrac
    else if y < 0 then
      (-x, -y)
    else
      (x, y) (* poderia até retornar um alias do argumento em vez de uma cópia *)

  let whole i = (i, 1)

  let add (a, b) (c, d) = ((a * d) + (c * b), b * d)

  let string_of_rational (n, d) =
    if n = 0 then
      "0"
    else 
      let rec gcd x y =
        if x = y then
          x
        else if x > y then
          gcd y x
        else
          gcd x (y - x)
      in
      let g = gcd (abs n) d in
      let num = n / g in
      let denom = d / g in
      string_of_int num
      ^
      if denom = 1 then
        ""
      else
        "/" ^ string_of_int denom
    
end

(* Note que usámos módulos explícitos e tipos de módulos para mostrar os 
conceitos, mas o mais comum em OCaml são os módulos implícitos:
  * foo.ml define um módulo Foo
  * se foo.mli estiver presente, é a assinatura de Foo, senão
    nada-é-escondido-de-outros-módulos (ainda escrevemos Foo.bar)
*)