(*Tiago Ribeiro a46346*)

(*Fontes consultadas:
  https://pt.wikipedia.org/wiki/Problema_da_mochila
  https://noic.com.br/materiais-informatica/curso/dp-02/
  https://chat.openai.com/chat (usado para ajudar com erro no print do lucro maximo)
*)


let n = read_int();; (* tamanho do bolo *)
let m = read_int();; (* numero de tamanhos de fatias *)

(* tamanho e preço das fatias *)
let precos = Array.make m (0, 0);;
if n,m<0 || n,m>10000 || m > n then failwith "Error" else
for x = 0 to m-1 do
  let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun i j -> (i, j)) in
  precos.(x) <- (tamanho, preco)
done;;

(* vetor dp -> lucro máximo *)
let dp = Array.make (n+1) 0;;
for j = 0 to m-1 do
  let tamanho, preco = precos.(j) in
  for i = tamanho to n do
    dp.(i) <- max dp.(i) (dp.(i-tamanho) + preco)
  done
done;;

(* print do lucro máximo *)
if n <= Array.length dp - 1 then
  let lucro_maximo = dp.(n) in
  Printf.printf "%d\n" lucro_maximo
else
  Printf.printf "Erro\n"