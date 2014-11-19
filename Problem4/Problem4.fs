module Problem4

let description = 
    "
    A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
    Find the largest palindrome made from the product of two 3-digit numbers.
    "

let rec NumberToList (n:int) =
    if (n > 0) then
        (n%10)::(NumberToList (n/10))
    else
        []            

let IsPalindrome l = 
    (List.rev l) = l

let Palindromes s e =
    [s..e]
    |> List.collect (fun x -> [for i in s..e -> (x * i, x, i)]) 
    |> List.filter (fun p ->
                            let x, _, _ = p 
                            IsPalindrome (NumberToList  x)
                    ) 
    |> List.max

let StartProblem = 
    printfn "%s" description
    printfn "%A" (Palindromes 100 999)

