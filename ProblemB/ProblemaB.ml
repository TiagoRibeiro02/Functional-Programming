let cut_cake n prices =
  let dp = Array.make_matrix (n+1) (n+1) 0 in
  for i = 1 to n do
    for j = 1 to n do
      if j < i then
        dp.(i).(j) <- dp.(i-1).(j)
      else
        dp.(i).(j) <- max dp.(i-1).(j) (dp.(i).(j-i) + prices.(i-1))
    done;
  done;
  let rec find_cuts i j acc =
    if i <= 0 || j <= 0 then
      acc
    else if dp.(i).(j) = dp.(i-1).(j) then
      find_cuts (i-1) j acc
    else
      find_cuts i (j-i) (i :: acc)
  in
  let cuts = find_cuts n n [] in
  dp.(n).(n), cuts

  let main=
  