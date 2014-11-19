module Problem6

let description = 
    "   
    The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385

    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 552 = 3025

    Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

    Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

    "

let sumOfSquares li =   
    li
    |> List.map (fun b -> b*b)
    |> List.sum    
  
let squareOfSum li =
    let sum = (List.sum li)
    sum*sum

let StartProblem = 
    printfn "%s" description
    let sqsum = squareOfSum [1..100]
    let sumsq =  sumOfSquares [1..100]
    printfn "%A" sqsum 
    printfn "%A" sumsq
    printfn "%A" (sqsum - sumsq)