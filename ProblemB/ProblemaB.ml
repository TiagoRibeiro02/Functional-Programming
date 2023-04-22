  let main=
  let n = Scanf.scanf "%d" (fun n -> n) (* leitura do tamanho do bolo *)
  let m = Scanf.scanf "%d" (fun m -> m) (* leitura do número de tamanhos de fatias *)
  (* leitura dos tamanhos e preços das fatias *)
  let fatias = Array.make m (0, 0)
  for i = 0 to m-1 do
    let tamanho, preco = Scanf.sscanf (read_line()) "%d %d" (fun x y -> (x, y)) in
    fatias.(i) <- (tamanho, preco)
  done