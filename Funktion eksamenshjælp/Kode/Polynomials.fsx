//Polynomials
type Poly = int list

//prune
let rec prune (l: int list) : Poly = 
    let l = List.rev l
    match l with
    | (x::tail) when x = 0 -> prune(List.rev(tail))
    | (x::tail) when x <> 0 -> List.rev(l)

//Add
let rec addh(p1: Poly, p2: Poly) : Poly = 
    match(p1, p2) with
    |   (_,[]) -> p1
    |   ([],_) -> p2
    |   (x::tail1, y::tail2) -> (x + y)::addh(tail1, tail2)

let add(p1: Poly, p2: Poly) : Poly = 
    prune(addh(p1,p2))

//MulC
let rec mulC (c:int, p:Poly) : Poly = 
    match(c, p) with
    |   (_,[]) -> p
    |   (0,_) -> []
    |   (_,x::tail) -> (c*x)::mulC(c,tail)


//Subtract
let rec subtractH (p1, p2: Poly) : Poly = 
    match(p1, p2) with
    |   ([],       y::tail2) -> (0 - y)::subtractH([],    tail2)
    |   (x::tail1,       []) ->       x::subtractH(tail1,    [])
    |   (x::tail1, y::tail2) -> (x - y)::subtractH(tail1, tail2)
    |   (_,[]) -> p1
    |   ([],_) -> p2

let subtract (p1, p2: Poly) : Poly = 
    prune(subtractH(p1,p2))

//MulX
let mulX (p:Poly) : Poly =
    //Tilføjer 0 i starten af listen, hvilket øger potensen for de efterfølgende tal
    match p with
    | [] -> []
    | _ -> [0] @ p

//printmul
let rec mulhelper(p : Poly) : Poly =
    match p with
    | (x::tail) when x = 0 -> mulhelper(tail@[x])
    | (x::tail) when x <> 0 -> List.rev(p)

//mul
let rec mul(p1, p2: Poly) : Poly =
    match(p1,p2) with
    |   (_,[]) -> []
    |   ([],_) -> []
    |   (x::tail,p2) -> add(mulC(x,p2),mulX(mul(tail,p2)))


//eval
let rec eval(c:int, p: Poly) : int =
    match(c,p) with
    |   (_,[]) -> 0
    |   (c,x::ps) -> x+c*(eval(c,ps))



//POLYNOMIALS 2
//isLegal
let isLegal (l: int list) : bool=
    let l = List.rev l
    match l with
    | (x::tail) when x = 0 -> false
    | (x::tail) when x <> 0 -> true





//toString
let rec toString (p: Poly) : string =
    let p = List.rev p
    match p with
    |   [] -> ""
    |   (ph::tail) when (tail.Length) = 0 && ph > 0 -> " + " + string ph + toString(List.rev(tail))
    |   (ph::tail) when (tail.Length) = 0 && ph < 0 -> " " + string ph + toString(List.rev(tail))
    |   (ph::tail) when (tail.Length) = 1 && ph > 0 -> " + " + string ph + "x" + toString(List.rev(tail))
    |   (ph::tail) when (tail.Length) = 1 && ph < 0 -> " " + string ph + "x" + toString(List.rev(tail))
    |   (ph::tail) when (tail.Length) > 1 && ph > 0 -> " " + string ph + "x^" + string ((p.Length)-1) + toString(List.rev(tail))
    |   (ph::tail) when (tail.Length) > 1 && ph < 0 -> " " + string ph + "x^" + string ((p.Length)-1) + toString(List.rev(tail))


//derivate
//Printer højeste potens først
let rec derivate (p: Poly) : Poly = 
    let p = List.rev p
    match p with
    | [] -> []
    //Hvis laveste potens skal printes først, skal List.rev slettes fra nedenstående linje
    | (ph::tail) when (tail.Length) = 0 -> List.tail(List.rev(p))
    | (ph::tail) when (tail.Length) > 0 -> [(tail.Length) * ph] @ derivate(List.rev(tail))

// Opløft polynomie p til c'e potens
let rec potensH (p: Poly, c) : Poly =
    match (p,c) with
    | ([],_) -> []
    | (_, c) when c > 1 -> (mul(p,potensH(p,c-1)))
    | (_, c) when c = 1 -> p

let potens (p:Poly ,c)= 
    potensH(p,c)
    // Comment out this line, to organize size of power
    // mulhelper(potensH(p,c))

//compose
//Hvad i alverden
let rec composeH (p1: Poly, p2: Poly) : Poly = 
    let p1 = List.rev p1
    match (p1, p2) with
    | ([],[]) -> []
    | (_,[]) -> p1
    | ([],_) -> p2
    | ((p1h::p1tail),(p2h::p2tail)) when p1tail.Length = 0 -> [0]
    | ((p1h::p1tail),(p2h::p2tail)) when (p2tail.Length > 0 && p1tail.Length > 0) -> add(mulC(p1h,potens(p2,(p1.Length)-1)),composeH(List.rev(p1tail),p2))

    
let compose (p1: Poly, p2: Poly) : Poly=
    match (p1, p2) with
    |   (p1h::tail1,p2h::tail2) -> [p1h+List.head(composeH(p1,p2))] @ List.tail(composeH(p1,p2))


//Opgave 3
//Basically den samme som isLegal, der tages bare højde for 
let isPolyLegal(p: Poly) : bool=
    let p = List.rev p
    match p with
    | [] -> true
    | (x::tail) when x = 0 -> false
    | (x::tail) when x <> 0 -> true


//Opgave 4
type Degree = | MinusInf of string
              | Fin of int

let deg (p:Poly) : Degree =
    match p with
    | [] -> MinusInf("")
    | _ when p.Length > 0 -> Fin(p.Length-1)


let addD (d1: Degree, d2: Degree) : Degree =
    match (d1, d2) with
    | (_,_) when d1 = MinusInf("") || d2 = MinusInf("") -> MinusInf("")
    | (Fin v1,Fin v2) when v1 >= 0 && v2 >= 0 -> Fin(v1+v2)



//Opgave 6
let ofList(l: int list): Poly =
    let p = l
    match p with
    | _ when p.Length = 0 -> (fail)with("Int list with no elements was entered")
    | _ when p.Length > 0 -> p


let toList(p: Poly): int list =
    let l = p
    match p with
    | _ when p.Length = 0 -> failwith("Poly with no elements was entered")
    | _ when p.Length > 0 -> p