//Opgave 5.1
//For at køre denne, skriv: filter (fun p -> p % 2 = 0) [x;y;z], hvis p%2=0, så fjerner filteret alle de ulige tal
let filter p xs = List.foldBack (fun f acc -> if p f then f::acc else acc) xs [];;

//Opgave 5.2
let revrev [xs;ys] = List.fold (fun acc x -> x :: acc) [] [List.rev xs;List.rev ys];;

//Opgave 5.3
//Virker ikke helt..
let sum p xs:int list = List.foldBack (fun f acc -> if p f then f + f::acc else acc) xs [];;

//Opgave 5.4
