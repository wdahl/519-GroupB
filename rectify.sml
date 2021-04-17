datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

fun union(nil, y) = y
    | union(x::l, y) = x::union(l, y);

fun remove(nil, n) = []
    | remove(z::l, n) = if z = n then remove(l, n)
                        else z::remove(l,n);

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