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
  O programa vai receber o "n" sendo este o tamanho do bolo e "m" o número de tamanhos de fatias, em seguida vai ler os tamanhos e preços de cada fatia, dando erro caso os valores não estejam dentro do limite 0 e 10000 ou caso "m" seja maior que "n".
 Depois vai criar um vetor "dp" com tamanho n+1 com o objetivo de guardar o lucro máximo calculado, sendo preenchido com    zeros ao início.
 O programa vai, para cada fatia, vai iterar sobre cada tamanho, e devolver o máximo entre o valor "i" calculado anteriormente e o valor do "i" atual, por exemplo, e usando o exemplo dado no enunciado, ao início dp(0)=0 e dp(1)=1 tamanho = 1 logo i = 1, dp(1-1)+2=0+2=2, logo dp(1)=2 em seguida i=2 calcula o máximo entre dp(2)=0 e dp(2-1)+2=2+2=4 logo dp(2)=4, quando "j" é incrementado, vai repetir o ciclo, mas desta vez dp(i) já terá valores para todos os i's e como o tamanho está na posição seguinte, ou seja, pelo exemplo, tamanho=2 logo "i" ira analisar entre dp(2) calculado na iteração anterior e dp(1)+preço e guardar o máximo entre esses valores, fazendo este ciclo até à última fatia, sendo que no final teremos dp(n)=lucro máximo.
 Finalmente vai dar print ao lucro máximo, tendo o cuidado de ver se "n" está dentro do intervalo valido.
*)