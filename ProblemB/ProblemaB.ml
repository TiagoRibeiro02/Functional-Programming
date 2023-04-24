let n = read_int();; (* tamanho do bolo *)
let m = read_int();; (* numero de fatias *)

(* tamanho e preço das fatias *)
let precos = Array.make m (0, 0);;
for i = 0 to m-1 do
  let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun x y -> (x, y)) in
  precos.(i) <- (tamanho, preco)
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
  Printf.printf "Erro: n fora dos limites do vetor dp\n"