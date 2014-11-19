module Problem3

open MathNet.Numerics

let description = 
    "
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
    "

// Se intenta usar la criba de Erastótenes
// el rendimiento es muy malo y se produce una excepción
let rec SievePrimeList (s:List<int64>) (stop:int64) (c:List<int64>) =
    let a = List.head s
    
    if a > stop then
        a::c
    else 
        let filtered = 
            List.tail s
            |> List.filter (fun b -> not(b % a = 0L))        

        SievePrimeList filtered stop (a::c)



// Comprobar si un número es primo con el te
let rec PrimeList (s:List<int64>) (stp:int64) =  
    let h = List.head s
    let t = List.tail s

    if (h * h > stp) then 
      s
    else 
      let nu = PrimeList (List.filter (fun b -> b % (h) <> 0L) t) stp
      List.append [h] nu
 
    
//let result = largestPrimeOf 600851475143L
let StartProblem =        
    printfn "%s" description
    let num = 600851475143L
    let result = 
        SievePrimeList [2L..(num-1L)] (775146L) []
    printfn "Resultado: %A" (List.max result)
 
 //775146