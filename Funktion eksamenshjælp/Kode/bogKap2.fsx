//Opgave 2.1
//f(24)=true
//f(27)=true
//f(29)=false
//f(30)=false

let f = function
    | 0 -> true
    | n when(n % 5)=0 -> false
    | n when(n % 2)=0 -> true
    | n when(n%3)=0 -> true
    | n -> false;;


//Opgave 2.2
let rec pow(s:string, n:int) =
    match (s,n) with
    | (s, 0) -> ""
    | (s, n)  -> string s + pow(s, n-1);;
  
//Opgave 2.3
let isIthChar(str:string, i:int, ch:char) = 
    match (str,i,ch) with
    | ("",i,ch) -> false
    | (str,i,ch) when (str.[i]=ch) -> true
    | (str,i,ch) -> false;;

//Opgave 2.4
let rec occFromIth(str:string, i:int, ch:char) =
    match (str,i,ch) with
    | (str,i,ch) when i>=str.Length -> 0
    | (str,i,ch) when str.[i]=ch -> 1 + occFromIth(str,i+1,ch);;
//Noter at den finder de karakterer der matcher efter den i'ende plads i strengen

//Opgave 2.5 IKKE FÆRDIG
(*
let i = 0;;
let res = 0;;
let rec occInString(str:string,ch:char) =
    match (str,ch) with
    | ("",ch) -> res
    | (str,ch) when i >= str.Length -> res
    | (str,ch) when str.[i]=ch -> i+1 res+1 (occInString(str,ch))
    | (str,ch) -> i+1 (occInString(str,ch));;
*)

//Opgave 2.6
let notDivisible(d:int,n:int) =
    match (d,n) with
    | (0,n) -> true
    | (d,n) when n%d=0 -> false
    | (d,n) -> true;;
   
//Opgave 2.7
//1.
let rec test(a:int,b:int,c:int) =
    if a <= b then 
        if notDivisible(a,c)=true then
            if a+1 = b then true else test(a+1,b,c)
        else false
    else false;;

//2. Hugget fra Birch/nettet
let prime (n:int) =
    match n with
    | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
    | _ ->
        let maxDiv = int(System.Math.Sqrt(float n)) + 1
        let rec f d i =
            if d > maxDiv then 
                true
            else
                if n % d = 0 then  
                    false
                else
                    f(d + i) (6 - 1)
        f 5 2;;

//3.
let rec nextPrime (n:int) =
    if prime(n+1) 
        then n+1
    else nextPrime(n+1);;


//Opgave 2.8
let rec bin (n:int, k:int) =
    match (n,k) with
    | (0,k) -> failwith "N må ikke være 0"
    | (n,k) when n<k -> failwith "N er mindre end k"
    | (n,0) -> 1
    | (n,k) when n=k || k=n -> 1
    | (n,k) -> bin(n-1,k-1) + bin(n-1,k);;


//Opgave 2.9
(*
1. int * int -> int
2. If x = 0 it stops and returns y
3. f(2,3) evaluation steps:

f(2,3)
f(2-1,2*3)      (1)
f(1-1,6*1)      (2)
f(0,6)          (3)
6
*)
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1,x*y);;

//Opgave 2.10
(*
1. bool * int -> int
2. 0
3. Den kører fact -1, som bliver en uendelig løkke
*)
let test2(c,e) = if c then e else 0;;


//Opgave 2.11 (100% = 1.00)
let VAT (n:int) (x:float) = (x + 1.0) * float(n);; 

let unVAT (n:int) (x:float) = x/float(n) - 1.0;;


//Opgave 2.12 (Lig mærke til parenteser i typen, de betyder det er en higher-order function)
//Kunne kun få typen her korrekt, ikke opgaven.
let min n = n (x:int) + (y:int);;


//Opgave 2.13
//Curry/uncurry er bare lort..