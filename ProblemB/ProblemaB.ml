  (* vetor dp para armazenar o lucro máximo *)
let dp = Array.make (n+1) 0

(* preenchimento do vetor dp *)
for j = 0 to m-1 do
  let tamanho, preco = precos.(j) in
  for i = tamanho to n do
    dp.(i) <- max dp.(i) (dp.(i-tamanho) + preco)
  done
done

(* impressão do lucro máximo *)
let lucro_maximo = dp.(n)
Printf.printf "%d\n" lucro_maximo

(* reconstrução da solução ótima *)
let rec reconstruir_solucao i fatias =
  if i = 0 then fatias else
  let tamanho, preco = precos.(fatias.(i)) in
  reconstruir_solucao (i - tamanho) (fatias.(i) :: fatias)

let fatias = Array.make (n+1) (-1)
let _ = dp.(n) |> ignore
let rec preencher_fatias i k =
  if k = 0 then () else
  if dp.(i) = dp.(i - precos.(k-1).(0)) + precos.(k-1).(1) then begin
    fatias.(i) <- k - 1;
    preencher_fatias (i - precos.(k-1).(0)) k
  end else
    preencher_fatias i (k - 1)
in
preencher_fatias n m;
let solucao = reconstruir_solucao n fatias in
Printf.printf "%s\n" (List.map string_of_int solucao |> String.concat " ")
  
  let main=
  let n = read_line()in (* leitura do tamanho do bolo *)
  let m = read_line()in (* leitura do número de tamanhos de fatias *)
  (* leitura dos tamanhos e preços das fatias *)
  let fatias = Array.make m (0, 0)in
  for i = 0 to m-1 do
    let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun x y -> (x, y)) in
    fatias.(i) <- (tamanho, preco)
  done