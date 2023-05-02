let rec lucro_maximo_rec n tp_fatias =
  let max_lucro = ref 0 in
  for j = 0 to Array.length tp_fatias - 1 do
    let tamanho, preco = tp_fatias.(j) in
    if tamanho <= n then
      let lucro = preco + lucro_maximo_rec (n - tamanho) tp_fatias in
      max_lucro := max !max_lucro lucro (*:= atualiza o valor do max_lucro e !max_lucro = valor atual do max_lucro*)
  done;;

let rec l_max i m =
  if (i != n+1) then 
    dp(i) = lmax (i+1) m
    for j = 0 to Array.length tp_fatias - 1 do
    let tamanho, preco = tp_fatias.(j) in
    if(m >= tamanho)then
      dp.(i) <- max dp.(i) (l_max((i+1) (m-tamanho)) + preco)
