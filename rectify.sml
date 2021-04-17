datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

fun union(nil, y) = y
                | union(x::l, y) = x::union(l, y);



fun FV (V x) = [(V x)]
                | FV (App (x,y)) = union((FV x),(FV y))
                | FV (Abs(n,z)) = remove((FV z),(V n))