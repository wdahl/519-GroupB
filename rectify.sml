datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

fun union(nil, y) = y
    | union(x::l, y) = x::union(l, y);

fun remove(nil, n) = []
    | remove(z::l, n) = if z = n then remove(l, n)
                        else z::remove(l,n);

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