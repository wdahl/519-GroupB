datatype Lambdaexp = V of int
                    | App of Lambdaexp * Lambdaexp
                    | Abs of int * Lambdaexp;

