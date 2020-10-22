//Opgave 1.1
let g n = n + 4;;

//Opave 1.2
let h (x,y) = System.Math.Sqrt(x**2.0+y**2.0);;

//Opgave 1.3
fun n -> n + 4;;

fun (x,y) -> System.Math.Sqrt(x**2.0+y**2.0);;

//Opave 1.4
let rec f = function
    | 0 -> 0
    | n -> n + f(n-1);;

(* Evaluation of f(4):
f(4)
4 + f(4-1)                  (1)
4 + f(3)                    (2)
4 + (3 + f(3-1))            (3)
4 + (3 + f(2))              (4)
4 + (3 + (2 + f(2-1)))      (5)
4 + (3 + (2 + (f(1))))      (6)
4 + (3 + (2 + (1 + f(1-1))))(7)
4 + (3 + (2 + (1 + f(0))))  (8)
4 + (3 + (2 + (1 + 0))))    (9)
4 + (3 + (2 + 1)))          (10)
4 + (3 + 3)                 (11)
4 + 6                       (12)
10                          (13)

*)

//Opgave 1.5
let rec F = function
    | 0 -> 0
    | 1 -> 1
    | n -> F(n-1) + F(n-2);;

(* Evaluation of F(4)
F(4)
F(4-1) + F(4-2)                       (1)
F(3) + F(2)                           (2)
(F(3-1) + F(3-2)) + (F(2-1)+F(2-2))   (3)
(F(2) + F(1)) + (F(1) + F(0))         (4)
(F(2-1) + F(2-2)) + 1) + (1 + 0)      (5)
(F(1) + F(0)) + 1) + 1                (6)
(1 + 0 + 1 + 1)                       (7)
3                                     (8)
*)

//Opgave 1.6
let rec sum = function
    | (m,0) -> m
    | (m,n) -> (m+n) + sum(m,n-1);;

(*
(m,0) = m               (Clause 1)
(m,n) = (m+(n-1))+(m+n) (Clause 2)
*)

//Opgave 1.7

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1);;


(*
(System,math.PI, fact-1) = float * int -> float

fact(fact 4) = int -> int -> int

power(System.Math.PI, fact 2) = float * int -> float

(power, fact) -> float * int -> float
*)

//Opgave 1.8
let a = 5;;
let k a = a + 1;;
let g b = (f b) + a;;

(*
    let a = 5;;
    let f a = a + 1;;
    let g b = (f b) + a;;

    env = [a -> 5]
          [f -> 6]
          [g -> (f b) + 5]



*)





