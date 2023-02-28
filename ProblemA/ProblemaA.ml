let rec f1 n=
  if n=0 then 1 
  else if n=1 then 2
  else 
    let rec soma k = if k > n - 2 then 0 else k + soma (f1(k) * f1(n-k-1)) in
     (3 * f1 (n-1)) + soma (f1(k) * f1(n-k-1))











let rec f2 n=
  if n=0 then 1 , 1
  else if n=1 then 2 , 1 
  else 
    let (a , b) , (c , d) = f2 (n-1) , f2 (n-2) in 
    ((((6 * n - 3) * a) - ((n - 2) * c)) / (n + 1) , b + d + 1)