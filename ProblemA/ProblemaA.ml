(*Tiago Ribeiro a46346*)

(*Fontes consultadas:
  https://en.wikipedia.org/wiki/Motzkin_number
  https://v2.ocaml.org/api/Hashtbl.html
  https://dsheets.github.io/codoc/zarith.1.3/z/
  https://chat.openai.com/chat (usado para ajudar num problema inicial, nomeadamente na redução da função)
*)

(*cria uma hash table*)
let hash = Hashtbl.create 1

(*função de Motzkin limitada entre 0 e 10000, onde os numeros pesquisados vão ser armazenados, podendo assim serem recuperados rapidamente e de forma eficiente*)
let rec fm n=
  let (&*) = Z.mul in
  let (&/) = Z.div in
  let (&+) = Z.add in

  if n<0 || n>10000 then failwith "Error" else 
  if n=0 then Z.one
  else if n=1 then Z.one
  else if Hashtbl.mem hash n then Hashtbl.find hash n
  else begin
    Hashtbl.add hash n
     (((Z.of_int (2*n+1) &* fm (n-1)) &+ ((Z.of_int (3*n-3) &* fm (n-2)))) &/ (Z.of_int (n+2)));

    Hashtbl.find hash n
  end

  (*função que lê o numero n dado pelo utilizador e dá print ao numero colculado pela função de Motzkin*)
  let main=
  let n = Scanf.scanf "%d" (fun n -> n)in
  Printf.printf "%s\n" (Z.to_string (fm (n)))

  (*O programa vai receber um numero n, em seguida, o mesmo é utilisado na função de Motzkin (fm), primeiro é analisado se o número está entre 0 e 10000, caso contrário falha, caso este esteja entre estes valores, caso n seja 0 ou 1 o valor dado pela função é 1, caso contrário, vai verificar que o valor já está presente na hash table, caso esteja devolve o seu valor, caso contrario é então aplicada a função de Motzkin, onde os valores dados pela mesma vão sendo adicionados à hash table, permitindo assim que quando ocorra a recursividade, os valores sejam recuperados de uma forma rapida, no final vai dar print ao valor esperado*)