(*Tiago Ribeiro a46346*)

(*Fontes consultadas:
  https://pt.wikipedia.org/wiki/Problema_da_mochila
  https://noic.com.br/materiais-informatica/curso/dp-02/
  https://chat.openai.com/chat (usado para ajudar com erro no print do lucro maximo)
*)

(* tamanho do bolo *)
let n = read_int();;
(* numero de tamanhos de fatias *) 
let m = read_int();; 

(* tamanho e preço das fatias *)
let tp_fatias = Array.make m (0, 0);;
if n<0 || n>10000 || m<0 || m>10000 || m > n then failwith "Error" else
for x = 0 to m-1 do
  let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun i j -> (i, j)) in
  tp_fatias.(x) <- (tamanho, preco)
done;;

(* vetor dp que vai guardar o lucro máximo calculado *)
let dp = Array.make (n+1) 0;;
for j = 0 to m-1 do
  let tamanho, preco = tp_fatias.(j) in
  for i = tamanho to n do
    Printf.printf "%d\n" i in
    dp.(i) <- max dp.(i) (dp.(i-tamanho) + preco)
  done
done;;

(* print do lucro máximo *)
if n <= Array.length dp - 1 then
  let lucro_maximo = dp.(n) in
  Printf.printf "%d\n" lucro_maximo
else
  Printf.printf "Error\n"

(*
  O programa vai receber o n sendo este o tamanho do bolo e m o numero de tamanhos de fatias, em seguida vai ler os tamanhos e preços de cada fatia, dando erro caso os valores não estejam dentro do limite 0 e 10000 ou caso m seja maior que n. 
  Depois vai criar um vetor "dp" com tamanho n+1 com o objetivo de guardar o lucro máximo calculado, sendo preenchido com zeros ao inicio.
  O programa vai, para cada fatia vai iterar sobre cada tamanho, e devolver o maximo entre o valor i calculado anteriormente e o valor do i atual, por exemplo ao inicio dp(0)=0
*)