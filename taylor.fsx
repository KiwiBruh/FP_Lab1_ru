open System

let a = 0.0
let b = 1.0
let eps = 0.000001
let delta = 0.1
let n = 10

let f x = sin x// my function

let rec factorial z = if z=1.0 then 1.0 else z * factorial (z-1.0)// needed for tailor

let dumb_tailor x =
    let rec tailor n final equas =
        if eps >= abs equas then final, n
        else
            let curFact = factorial(2.0*n+1.0)
            let equas = ((((-1.0) ** n) * (x ** (2.0*n + 1.0))) / curFact)
            tailor (n + 1.0) (final + equas) equas
    tailor 1 x (x)

let smart_tailor x =
    let rec tailor_2 n final equas =
        if eps >= abs equas then final, n + 1.0
        else
            let equas = equas * (-1.0)*(x**2.0)/((2.0 * n + 2.0) * (2.0 * n + 3.0))
            tailor_2 (n + 1.0) (final + equas) equas
    tailor_2 0 (x) (x)

let main =
    let rec table a b =
        if a > b then printfn("finished")
        else
            let dumb, n1 = dumb_tailor a
            let smart, n2 = smart_tailor a
            printfn "|%5.1f | %f | %f | %.0f | %f | %.0f |" a (f a) smart n2 dumb n1
            table (a + delta) b
    table a b
