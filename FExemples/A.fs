

let g = fun x -> (fun y -> x+y)

let g x y = x+y

let r = g 4
r 1


let a = 2

let f x = a+x

f 5

let a = 6

let rec f n = 
    if n = 0 
    then printf "fini\n"
    else printf "salut\n"
         f (n-1)


