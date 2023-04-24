let m = read_int() in

let precos = Array.make m (0, 0) in
for i = 0 to m-1 do
  let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun x y -> (x, y)) in
  precos.(i) <- (tamanho, preco)
done