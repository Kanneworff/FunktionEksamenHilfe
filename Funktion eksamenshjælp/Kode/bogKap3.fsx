//Opgave 3.1

//Triples
let tp1 = (11,15,"AM");;
let tp2 = (11,15,"AM");;

let timeTesterTriples (tuple1, tuple2) = 
    match (tuple1, tuple2) with
    | ((_,_,"AM"),(_,_,"PM")) -> printfn "%A comes before %A" tuple1 tuple2
    | ((_,_,"PM"),(_,_,"AM")) -> printfn "%A comes before%A" tuple2 tuple1
    | ((h1,_,_),(h2,_,_)) when(h1 < h2) -> printfn "%A comes before %A" tuple1 tuple2
    | ((h1,_,_),(h2,_,_)) when(h2 < h1) -> printfn "%A comes before %A" tuple2 tuple1
    | ((_,m1,_),(_,m2,_)) when(m1 < m2) -> printfn "%A comes before %A" tuple1 tuple2
    | ((_,m1,_),(_,m2,_)) when(m2 < m1) -> printfn "%A comes before %A" tuple2 tuple1
    | ((_,_,_),(_,_,_)) -> printfn "%A and %A is the same time" tuple1 tuple2;;

//Records
type record = {hours : int; minutes : int; f : string};;
let rc1 = {hours = 9; minutes = 10; f = "AM"};;
let rc2 = {hours = 9; minutes = 10; f = "AM"};;

let timeTesterRecord(record1, record2) =
    if record1.f = "AM" && record2.f = "PM"
        then printfn "%A \n comes before \n %A" record1 record2
    else if record1.f = "PM" && record2.f = "AM"
        then printfn "%A \n comes before \n %A" record2 record1
    else if record1.hours < record2.hours
        then printfn "%A \n comes before \n %A" record1 record2
    else if record2.hours < record1.hours
        then printfn "%A \n comes before \n %A" record2 record1
    else if record1.minutes < record2.minutes
        then printfn "%A \n comes before \n %A" record1 record2
    else if record2.minutes < record1.minutes
        then printfn "%A \n comes before \n %A" record2 record1
    else printfn "%A \n and \n %A \n is the same time!" record1 record2;;
   
//Opgave 3.2
let tp3 = (1,19,0);;
let tp4 = (2,0,12);;
let rec overflow(a:int,b:int,c:int) = function;;


//Adds the two triple'd values
let addCurrency(tuple1,tuple2) =

    let (pounds1, shillings1, pence1) = tp3
    let (pounds2, shillings2, pence2) = tp4
    let (pounds3, shillings3, pence3) = (pounds1+pounds2, shillings1 + shillings2, pence1 + pence2)

    if pence3 < 12 && shillings3 < 20
    then printfn "%A pounds, %A shillings and %A pence" pounds3 shillings3 pence3
    else overflow(pounds3,shillings3,pence3);;

//Subtracts the two tripple'd values
let subtractCurrency(tuple1,tuple2) =

    let (pounds1, shillings1, pence1) = tp3
    let (pounds2, shillings2, pence2) = tp4
    let (pounds3, shillings3, pence3) = (pounds1 - pounds2, shillings1 - shillings2, pence1 - pence2)

    if pence3 < 12 && shillings3 < 20 && (pence3 >= 0 && shillings3 >= 0 && pounds3 >= 0)
    then printfn "%A pounds, %A shillings and %A pence" pounds3 shillings3 pence3
    else overflow(pounds3,shillings3,pence3);;
    
//Checks if the currency exceeds its own capacity (12 pence = 1 shilling) -> (36 pence = 3 shillings) etc
let rec overflow(pounds,shillings,pence) =

    if pence >= 12
    then overflow(pounds,shillings+1,pence-12)
    else if shillings >= 20
    then overflow(pounds+1,shillings-20,pence)
    else if pence < 0
    then overflow(pounds,shillings-1,pence+12)
    else if shillings < 0
    then overflow(pounds-1,shillings+20,pence)
    else if pounds < 0
    then printfn "You have %A pounds!! " pounds
    else printfn "%A pounds, %A shillings and %A pence" pounds shillings pence;;


//Opgave 3.3
//3.3.1
let pair1 = (2,4);;
let pair2 = (1,2);;

let addPair (pairA,pairB) =
    let (a,b) = pairA
    let (c,d) = pairB
    let (r1,r2) = (a+c,b+d)
    let result = (r1,r2)
    printfn "%A + %A = %A" pairA pairB result

let mulPair (pairA,pairB) =
    let (a,b) = pairA
    let (c,d) = pairB
    let (r1,r2) = (a*c - b*d,b*c + a*d)
    let result = (r1,r2)
    printfn "%A + %A = %A" pairA pairB result

//3.3.2
let subPair (pairA,pairB) =
    let (a,b) = pairA
    let (c,d) = pairB
    let (r1,r2) = (a-c,b-d)
    let result = (r1,r2)
    printfn "%A + %A = %A" pairA pairB result


//Alt herfra er lort...
let divPair (pairA,pairB) =
    let (a,b) = pairA
    let (c,d) = pairB
    let (r1,r2) = (a,d)
    let result = (r1,r2)
    printfn "%A + %A = %A" pairA pairB result