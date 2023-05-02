let rec lucro_maximo_rec n tp_fatias =
  if n <= 0 then 0 else
  let max_lucro = ref 0 in
  for j = 0 to Array.length tp_fatias - 1 do
    let tamanho, preco = tp_fatias.(j) in
    if tamanho <= n then
      let lucro = preco + lucro_maximo_rec (n - tamanho) tp_fatias in
      max_lucro := max !max_lucro lucro (*:= atualiza o valor do max_lucro e !max_lucro = valor atual do max_lucro*)
  done;;