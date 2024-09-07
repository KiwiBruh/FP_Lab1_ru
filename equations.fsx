open System

let cotan x = 1.0/tan(x)
let acotan x = Math.PI - atan(x)

let f1 x = x * tan(x) - (1.0/3.0)
let f2 x = tan(x/2.0) - cotan(x/2.0) + x
let f3 x = 0.4 + atan(x**0.5) - x

let f1' x = (cos(x)*sin(x) + x * 1.0) / cos(x) ** 2.0
let f2' x = 1.0 + 1.0/ (2.0 * (cos(x/2.0) ** 2.0)) + 1.0/ (2.0 * (sin(x/2.0) ** 2.0))
let f3' x = 1.0 / (2.0 * (x ** 0.5) + 2.0 * x * (x ** 0.5)) - 1.0

let phi1 x = cotan(x)/3.0
let phi2 x = cotan(x/2.0) - tan(x/2.0)
let phi3 x = 0.4 + atan(x**0.5)

let eps =0.0001

let rec dichotomy f a b =
    let c = (a+b) / 2.0 
    if abs(f c) < eps then c
    else
        if abs((f a)*(f c)) = ((f a)*(f c)) then
            dichotomy f c b
        else
            dichotomy f a c

let rec iterations phi a =
    if abs((phi a) - a) < eps then a
        else
            iterations phi (phi a)
    

let newthon f f' b =
    let newt b = b - (f b)/(f' b)
    iterations newt b

let main = 
    printfn " |%10.5f | %10.5f | %10.5f |" (dichotomy f1 0.2 1.) (iterations phi1 1.) (newthon f1 f1' 1.0)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2 1. 2.) (iterations phi2 2.) (newthon f2 f2' 2.0)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 1. 2.) (iterations phi3 2.) (newthon f3 f3' 2.)
