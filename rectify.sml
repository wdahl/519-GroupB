datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

fun union(nil, y) = y
    | union(x::l, y) = x::union(l, y);

fun remove(nil, n) = []
    | remove(z::l, n) = if z = n then remove(l, n)
                        else z::remove(l,n);

fun contains((V x), nil) = false
    | contains((V x), (V y)::l) = if x = y then true
                        else contains((V x), l);

fun double(nil) = false
    | double((V x)::l) = if contains((V x), l) then true
                    else double(l);

fun BV (V x) = []
    | BV (App (x,y)) = union((BV x), (BV y))
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

fun rectify (V x) = (V x)
    | rectify (App(u,v)) = App((rectify u), (rectify v))
    | rectify (Abs(m,u)) = let
                            val r = newvarnum (Abs(m,u))
                            val uu = replacefvar m r u
                        in 
                            uu
                        end;