let iterativeImprove isGoodEnough improve =
    let rec iter guess =
        if isGoodEnough guess then
            guess
        else
            iter (improve guess)
    iter 1.0

let square n = pown n 2
let average x y = (x + y)  / 2.0

let sqrt x =
    iterativeImprove
        (fun guess -> ((abs ((square guess) - x)) < 0.0001))
        (fun guess -> (average guess (x / guess)))

sqrt 16.0