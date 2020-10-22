//Opgave 4.1
let upto n =
    match n with
    | n -> [1..n];;

//Opgave 4.2
let downto1 n =
    match n with
    | n -> [n..(-1)..1];;

//Opgave 4.3
let rec evenN n =
    match n with
    | 0 -> []
    | n when n % 2 = 0 -> n::(evenN(n-1))
    | n -> evenN(n-1)

//Opgave 4.4
let rec altsum = function
    | [] -> 0
    | x0::xs -> x0 - altsum xs;;

//Opgave 4.5
let rec rmOdd = function
    | [] -> []
    | x::xs when not(x % 2 = 0) -> rmOdd(xs)
    | x::xs -> x::rmOdd(xs);;

//Opgave 4.6
let rec rmEven = function
    | [] -> []
    | x::xs when x % 2 = 0 -> rmEven(xs)
    | x::xs -> x::rmEven(xs);;

//Opgave 4.7
let rec multiplicity (x,xs) = 
    match xs with
    | [] -> 0
    | n::ns when n = x -> 1 + multiplicity(x,ns)
    | n::ns -> multiplicity(x,ns);;

//Opgave 4.8
let split list = 
    let rec loop l (xs, ys) = 
        match l with
        |x::y::rest -> loop rest (x::xs,y::ys)
        | [x] -> (x::xs,ys)
        | _ -> (xs,ys)
    loop list ([],[]);;

//Opgave 4.9
let rec zip (list1:int list, list2:int list) =
   match (list1,list2) with
   | ([],[]) -> []
   | (_,[]) -> failwith "List 1 is longer than list 2"
   | ([],_) -> failwith "List 2 is longer than list 1"
   | (x::xtail,y::ytail) -> [(x,y)] @ zip(xtail,ytail);;

//Opgave 4.10
let rec equals list1 list2 = 
    match (list1,list2) with
    | ([], _) -> true
    | (x::xtail, y::ytail) when x = y -> equals xtail ytail
    | (list1,list2) -> false;;

let prefix listM listN = 
    match (listM,listN) with
    | ([],[]) -> true
    | (listM, listN) when listM.Length <= listN.Length -> equals listM listN
    | (listM, listN) -> false;;

//Opgave 4.11.1
let rec count xs s =
    match (xs,s) with
    | ([],_) -> 0
    | (x::tail, s) when x = s -> 1 + count tail s
    | (x::tail, s) -> count tail s;;

//Opgave 4.11.2
let rec insert xs s =
    match (xs, s) with
    | ([], 0) -> []
    | ([], s) -> [s]
    | (x::tail, s) when x = s -> [s] @ [x] @ insert tail 0
    | (x::tail, s) when s < x -> [s] @ [x] @ insert tail 0
    | (x::tail, s) -> [x] @ insert tail s;;

//Opgave 4.11.3
let rec intersect (xs:int list,ys:int list) =
    match (xs,ys) with
    | ([],[]) -> []
    | ([], y::ytail) -> []
    | (x::xtail, []) -> []
    | (x::xtail, y::ytail) when x < y -> intersect(xtail,ys)
    | (x::xtail, y::ytail) when x > y -> intersect(xs,ytail)
    | (x::xtail, y::ytail) when x = y -> [x] @ intersect(xtail,ytail)
    | (x::xtail, y::ytail) -> intersect(xtail,ytail);;

//Opgave 4.11.4
let rec plus (xs:int list, ys:int list) =
    match (xs,ys) with
    | ([],[]) -> []
    | (x::xtail, []) -> xs
    | ([], y::ytail) -> ys
    | (x::xtail, y::ytail) when x < y -> [x] @ plus(xtail,ys)
    | (x::xtail, y::ytail) when x = y -> [x] @ plus(xtail,ys)
    | (x::xtail, y::ytail) when x > y -> [y] @ plus(xs,ytail)
    | (x::xtail,y::ytail) -> plus(xtail,ytail);; //Den burde aldrig nå hertil, men brokkede sig over incomplete matches uden.

//Opgave 4.11.5
let rec minus (xs:int list, ys:int list) =
    match (xs,ys) with
    | ([],[]) -> []
    | (xs, []) -> xs
    | ([], ys) -> []
    | (x::xtail, y::ytail) when x = y -> minus(xtail, ytail)
    | (x::xtail, y::ytail) when x < y -> [x] @ minus(xtail, ys)
    | (x::xtail, y::ytail) when x > y -> minus(xs, ytail)
    | (x::xtail, y::ytail) -> minus(xtail,ytail);;

//Opgave 4.12
//Ikke helt sikker på hvad de mener, så laver den med summen af de tal der opfylder f.eks. n % p = 0, hvor p er predicated.
let rec sum(p,xs) =
    match (p,xs) with
    | (0,xs) -> 0
    | (p,[]) -> 0
    | (p,x::xtail) when x % p = 0 -> x + sum(p,xtail)
    | (p,x::xtail) -> sum(p,xtail);;

//Opgave 4.13.1
let rec finderH (s:int, xs:int list) =
    match (s,xs) with
    | (s,[]) -> [s]
    | (s, x::xtail) when s <= x -> finderH(s,xtail)
    | (s, x::xtail) when s > x -> finderH(x,xtail)
    | (s, x::xtail) -> finderH(s,xtail);;

let findS xs:int list =
    match xs with
    | [] -> failwith "Tomt array"
    | x::xtail -> finderH(x, xtail);;

//Opgave 4.13.2
let rec delete (a:int, xs:int list) : int list=
    match (a,xs) with
    | (s,[]) -> []
    | (0, xs) -> xs
    | (s, x::xtail) when s = x -> delete(0, xtail) //Arrayed må ikke indeholde 0, kunne ikke finde på andre måder at løse denne opgave på
    | (s, x::xtail) -> [x] @ delete(s,xtail);;

//Opgave 4.13.3
//Meget ineffektiv løsning, kalder 2 gamle funktioner og 1 modificeret funktion
let rec finderHS (s:int, xs:int list) =
    match (s,xs) with
    | (s,[]) -> [s]
    | (s, x::xtail) when s <= x -> finderHS(s,xtail)
    | (s, x::xtail) when s > x -> finderHS(x,xtail)
    | (s, x::xtail) -> finderHS(s,xtail);;

//Havde behov for at finde det mindste element som int og ikke liste med 1 element
//Så duplikerede ovenstående finderHS til at give en int
let rec finderHM (s:int, xs:int list) =
    match (s,xs) with
    | (s,[]) -> s
    | (s, x::xtail) when s <= x -> finderHM(s,xtail)
    | (s, x::xtail) when s > x -> finderHM(x,xtail)
    | (s, x::xtail) -> finderHM(s,xtail);;

let rec sorter xs:int list =
    match xs with
    | [] -> []
    | x::xtail -> finderHS(x,xtail) @ sorter(delete(finderHM(x,xtail),xs));;


//Opgave 4.14 
let rec smallestO = function
    | [] -> None
    | x::xtail -> Some(finderHM(x,xtail));;

//Opgave 4.15
//Virker kun hvis den yderste liste indeholder 2 lister, ellers bliver den meget sur...
let revrev [xs:int list; ys:int list] =
    match [xs;ys] with
    | [[];[]] -> [[];[]]
    | [xs;[]] -> List.rev [List.rev xs;[]]
    | [[];ys] -> List.rev [[];List.rev ys]
    | [xs;ys] -> List.rev [List.rev xs;List.rev ys];;

//Opgave 4.16
(*
    Types:
    f = int * int list -> int list  Korrekt
    
    g = 'a * 'b list -> ' a list    Forkert
    g = ('a * 'a) list -> ('a * 'a) list    Korrekt

    h = 'a list -> 'a * 'a list    Forkert
    h = 'a list -> 'a list   Korrekt

    Values:
    1. The function f takes a value x, and a list ys, that cant be negative.
       It then adds the value for x and the head of ys, and makes that the head of a new list, with
       x-1 as new x value and the tail ys as the new array. When it calls the function, it keeps the first head, and
       then adds new heads, but they come after the first in the array.

       Example:
       f (5,[1;3;5]):
       5+1::[f(5-1,[3;5])]
       4+3::[6;f(4-1,[5])]
       3+5::[6;7;f(3-1,[])]
       8::[6;7]
       [6;7;8]

    2. It takes a list of pairs. It then takes out the head pair, and tails the remainder of the list.
       The function then first heads the original pair (x,y), followed by a reversed pair (y,x).
       Then i calls the function again with the tail as new array.
       Note that the array becomes twice as long as the original.

       Example:
       g [(x,y);(z,k)]
       (x,y)::(y,x)::g [(z,k)]
       [(x,y);(y,x);(z,k);(k,z)]

    3. The function h takes a list. It then takes the head x and tail xs of said list.
       The head x becomes head of a new list, but it also becomes the last element of that list.
       In between the head and last element, the function is called again, with the tail xs as the new list.
       For every 1 element in the original list, 2 elements are added to the new list.

       Example:
       h [1;2;3]:
       1::h [2;3] @ [1]
       [1;2::h[3] @ [2];1]
       [1;2;3::h[] @ [3];2;1]
       [1;2;3;3;2;1]
       *)

let rec f = function
    | (x, []) -> []
    | (x,y::ys) -> (x+y)::f(x-1,ys);;

let rec g = function
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s;;

let rec h = function
    | [] -> []
    | x::xs -> x::(h xs) @ [x];;


//Opgave 4.17
(*
    Type = 'a list -> 'a list    Forket
    Type = ('a -> bool) -> 'a list -> 'a list    Korrekt   (Bemærk, hvis if/else skal bool vist indgå i typen, ikke 100 på den dog)

    Value:
    Den tager en liste, og laver x til head og xs til tail.
    Derefter laver den en ny værdi ys, som bliver funktion med xs som input.
    Hvis q af x er korrekt, vil den lave en ny liste med x som head og ys som tail, ellers laver listen med ys og x som sidste element.
    
    Kan ikke tjekke den, kan ikke få funktionen til at køre...
*)

let rec p q = function
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys@[x];;


//Opgave 4.18
(*

    Type = 'a list -> 'a list   Forkert
    Type = ('a -> 'a) -> 'a list -> 'a list   Korrekt



*)

let rec j g = function
    | [] -> []
    | x::xs -> g x :: j (fun y -> g(g y)) xs;;

//Opgave 4.19
(*
    The areNb m c1 c2 checks whether 2 countries are neighbours.
    It does this by checking first if c1,c2 are neighbours or if c2,c1 are neighbours.
    This means, that if the first check c1,c2 comes back false, it will then check c2,c1.
    This is unessesary since if c1,c2 arent neighbours, c2,c1 wont be neighbours either.

    Alternative declaration:
    let areNb m c1 c2 = 
        isMember (c1,c2) m;;
*)

//Opgave 4.20

(*
let colMap m = 
    let areNb c1 c2 = isMember (c1,c2) m;;
    let canBeExtBy col c = 
        match col with
        | [] -> true
        | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c;;
    let rec extColouring m cols c =
        match cols with
        | [] -> [[c]]
        | col::cols' -> if canBeExtBy m col c
                        then (c::col)::cols'
                        else col::extColouring m cols' c;;
    
*)

(*
    Opgave 4.21 er lort
    Opgave 4.22 er bare polynomier
    Opgave 4.23 er lort
*)



