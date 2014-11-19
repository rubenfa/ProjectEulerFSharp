module Problem5


let description = 
    "   

    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

    "
      
let rec Divisible x li =
    if (List.forall (fun b -> x % b = 0) li) then
        x
    else
        Divisible (x+1) li
   

let StartProblem = 
    printfn "%s" description
    printfn "%A" (Divisible 21 [1..20])
