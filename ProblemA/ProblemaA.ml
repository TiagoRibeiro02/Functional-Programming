

(*funções de input que dada uma linha, divide-a pelo espaço e retorna dois inteiros a e b*)
  let line = read_line ();;
  let line_parts = String.split_on_char ' ' line;; 
  let a = int_of_string (List.nth line_parts 0);; 
  let b = int_of_string (List.nth line_parts 1);;

(*função recursiva com a função auxiliar somatorio*)
(*função recursiva do somatório da equação*)
let count= ref 0;;
let rec f1 n=
  count := !count+1;
  let rec soma n =
    let rec soma_aux k n acc =
      if k > n-2 then acc 
      else soma_aux (k+1) n (acc+(f1(k)*f1(n-k-1))) in 
    soma_aux 1 n 0 in
  if n=0 then 1 
  else if n=1 then 2
  else 3* (f1 (n-1)) + (soma (n))


let count2 = ref 0;;

let rec f2 n=
  count2 := !count2 + 1;
  if n = 0 then Z.one
  else if n=1 then Z.of_int 2
  else ((((6*n-3)*f2 (n-1)) - ((n-2)*f2 (n-2)))/(n+1))