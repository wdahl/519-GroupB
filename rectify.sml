(*  Group B *)

(*  William Dahl 
    wdahl@albany.edu
    wd347166    *)
    
(*  Pooja Lokhandwala
    PL443414
    plokhandwala@albany.edu  *)
  
datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

fun removeDups [] = []
    | removeDups (x::l) = x::removeDups(List.filter (fn y=> y <> x) l);

fun concat(nil, y) = y
    | concat(x::l, y) = x::concat(l, y);

fun union(x, y) = removeDups (concat(x,y));

fun remove(nil, n) = []
    | remove(z::l, n) = if z = n then remove(l, n)
                        else z::remove(l,n);

fun contains(x, nil) = false
    | contains(x, y::l) = if x = y then true
                        else contains(x, l);

fun double(nil) = false
    | double(x::l) = if contains(x, l) then true
                    else double(l);

fun BV (V x) = []
    | BV (App (x,y)) = concat((BV x), (BV y))
    | BV (Abs (n,z)) = (V n)::(BV z);

fun FV (V x) = [(V x)]
    | FV (App (x,y)) = union((FV x),(FV y))
    | FV (Abs (n,z)) = remove((FV z),(V n));

fun replacefvar x y (V z) = if x = z then (V y) else (V z)
    | replacefvar x y (Abs (n,z)) = if x = n
                                    then 
                                        Abs (n,z)
                                    else
                                        Abs (n, (replacefvar x y z))
    | replacefvar x y (App (u,v)) = App ((replacefvar x y u),
                                        (replacefvar x y v));

fun max [(V x)] = x
    | max ((V x)::l) = let 
                            val y = max l
                        in
                            if x > y then x else y
                        end;

fun newvarnum exp = (max (union((FV exp), (BV exp)))) + 1;

fun alphaequal (V i) (V j) = (i = j)
    | alphaequal (App(u,v)) (App(x,y)) = (alphaequal u x) andalso (alphaequal v y)
    | alphaequal (Abs(m,u)) (Abs(n,v)) = if m = n then (alphaequal u v)
                                            else 
                                                let
                                                    val r = newvarnum (App(Abs(m, u), Abs(n, v)))
                                                    val uu = replacefvar m r u
                                                    val vv = replacefvar n r v
                                                in
                                                    (alphaequal uu vv)
                                                end
    | alphaequal _ _ = false;

fun rectified exp = if double(concat((FV exp), (BV exp))) = false 
                    andalso double((BV exp)) = false
                    then true
                    else false;

fun changeVars (V x) = (V x)
    | changeVars (App(u,v)) = App((changeVars u), (changeVars v))
    | changeVars (Abs(m,u)) =   let
                                    val r = newvarnum (Abs(m,u))
                                    val uu = replacefvar m r u
                                in
                                    Abs(r, (changeVars uu))
                                end;

fun rectify (V x) = (V x)

    | rectify (App(u,v)) =  if rectified (App(u,v)) = true 
                            then App(u,v)

                            else if rectified u = true andalso rectified v = true
                            then rectify (App(u, (changeVars v)))

                            else if rectified u = true 
                            then rectify (App(u, (rectify v)))

                            else if rectified v = true
                            then rectify (App((rectify u), v))

                            else rectify (App((rectify u), (rectify v)))       

    | rectify (Abs(m,u)) = if rectified (Abs(m,u)) = true then Abs(m,u)
                            else
                                let
                                    val r = newvarnum (Abs(m,u))
                                    val uu = replacefvar m r u
                                in
                                    rectify (Abs(r, (rectify uu)))
                                end;
