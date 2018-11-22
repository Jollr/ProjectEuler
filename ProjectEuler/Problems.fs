module Problems

open System

let Problem1 () =
   [1 .. 999]
   |> Seq.filter (fun n -> n % 3 = 0 || n % 5 = 0)
   |> Seq.reduce (fun a b -> a + b)
   |> (fun a -> a.ToString())

let Problem2 () =
    let lazyFib = 
        Seq.unfold 
            (fun (n1, n2) -> 
                let next = (n1 + n2)
                if (next > 4000000) then None
                else Some(next, (n2, next))
            )
            (0, 1)
            
    lazyFib
        |> Seq.filter (fun n -> n % 2 = 0)
        |> Seq.reduce (fun a b -> a + b)
        |> (fun a -> a.ToString())

let Problem3 () =
    let input : int64 = 600851475143L

    let primesSteps = 
        Seq.unfold
            (fun (result : List<int64>, number : int64, divisor : int64) ->
                if (number = 1L)
                then None
                else 
                    if (number % divisor = 0L)
                    then Some((List.append result [divisor]), ((List.append result [divisor]), number / divisor, divisor))
                    else Some(result, (result, number, divisor + 1L))
            )
            (list.Empty, input, 2L)
    
    let last = Seq.last primesSteps
    let strings = Seq.map (fun a -> a.ToString()) last
    String.concat "," strings

let Problem4 () = 
    Seq.collect 
        (fun n1 -> (Seq.map (fun n2 -> n1*n2) [ 100 .. 999 ]))
        [ 100 .. 999 ]
    |> Seq.sortBy id
    |> Seq.distinct
    |> Seq.map (fun a -> a.ToString())
    |> Seq.filter (fun s -> String.Compare(s, new string(Array.rev (s.ToCharArray()))) = 0)
    |> String.concat ","

let Problem5 () = 
    let isAwesome number = 
        Seq.map (fun divisor -> number % divisor) [2 .. 20]
        |> Seq.filter (fun n -> n > 0)
        |> Seq.isEmpty
    
    let candidates = Seq.initInfinite(fun n -> (n + 1) * 20)
    let result = Seq.filter isAwesome candidates |> Seq.head
    result.ToString()

let Problem6 () = 
    let sum = Seq.reduce (fun a b -> a + b) [1 .. 100] 
    let squareOfSums = sum * sum
    let sumOfSquares = Seq.reduce (fun a b -> a + b * b) [1 .. 100] 
    let result = squareOfSums - sumOfSquares
    result.ToString()

let Problem7 () =
    let isPrime n = 
        if (n % 2 = 0 || n % 3 = 0 || n % 5 = 0 || n % 7 = 0)
        then false
        else  
            [ 7 .. n / 7]
            |> Seq.filter (fun divisor -> n % divisor = 0)
            |> Seq.isEmpty


    let candidates = 
        Seq.unfold
            (fun (x, y) -> 
                match y with
                    | 1 -> Some (x, (x + 2, 3))
                    | 3 -> Some (x, (x + 4, 7))
                    | 7 -> Some (x, (x + 2, 9))
                    | 9 -> Some (x, (x + 2, 1))
            )
            (11, 1)

    Seq.filter isPrime candidates
    |> Seq.append [2; 3; 5; 7]
    |> Seq.skip 10000 
    |> Seq.head
    |> (fun x -> x.ToString())


let Problem8 () =
    let input = 
        "73167176531330624919225119674426574742355349194934" +
        "96983520312774506326239578318016984801869478851843" +
        "85861560789112949495459501737958331952853208805511" +
        "12540698747158523863050715693290963295227443043557" +
        "66896648950445244523161731856403098711121722383113" +
        "62229893423380308135336276614282806444486645238749" +
        "30358907296290491560440772390713810515859307960866" +
        "70172427121883998797908792274921901699720888093776" +
        "65727333001053367881220235421809751254540594752243" +
        "52584907711670556013604839586446706324415722155397" +
        "53697817977846174064955149290862569321978468622482" +
        "83972241375657056057490261407972968652414535100474" +
        "82166370484403199890008895243450658541227588666881" +
        "16427171479924442928230863465674813919123162824586" +
        "17866458359124566529476545682848912883142607690042" +
        "24219022671055626321111109370544217506941658960408" +
        "07198403850962455444362981230987879927244284909188" +
        "84580156166097919133875499200524063689912560717606" +
        "05886116467109405077541002256983155200055935729725" +
        "71636269561882670428252483600823257530420752963450" 
    
    let total substring = 
        Seq.toList substring 
        |> Seq.map int64 
        |> Seq.map (fun a -> a - 48L)
        |> Seq.reduce (fun a b -> a*b)

    //let substrings = Seq.map (fun n -> input.[n .. (n + 12)]) [0 .. input.Length - 13] 
    //for s in substrings do printfn "%s" (s + " -> " + ((total s).ToString()))

    Seq.map (fun n -> input.[n .. (n + 12)]) [0 .. input.Length - 13] 
    |> Seq.map total
    |> Seq.max
    |> (fun x -> x.ToString())

let Problem9 () =
    let append v x = Seq.map (fun vn -> (x, vn)) v
    let carth v1 v2 = Seq.collect (fun x -> append v2 x) v1
    
    [1 .. 1000]
    |> carth [1 .. 1000]
    |> carth [1 .. 1000]
    |> Seq.filter (fun (c, (a,b)) -> (a + b + c = 1000))
    |> Seq.filter (fun (c, (a,b)) -> (a*a + b*b = c*c))
    |> Seq.head
    |> (fun x -> x.ToString())

//let Problem10 =
//    let candidates = 
//        Seq.unfold
//            (fun (x, y) ->
//                if x > 2000000
//                then None
//                else match y with
//                    | 1 -> Some (x, (x + 2, 3))
//                    | 3 -> Some (x, (x + 4, 7))
//                    | 7 -> Some (x, (x + 2, 9))
//                    | 9 -> Some (x, (x + 2, 1))
//                    | 2 -> Some (x, (x + 1, 3))
//            )
//            (2, 2)

//    let hasDivisors a b = 
//        Seq.filter (fun n -> b % n = 0) a
//        |> Seq.isEmpty
//        |> not

//    // kan niet met reduce?
//    Seq.reduce 
//        (fun a ( b : int ) ->
//            if (hasDivisors b a)
//            then a
//            else (Seq.concat a b)
//        )
//        []
//        candidates


