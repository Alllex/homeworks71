(*
    Problem: LongArithmetic
    Author: Alex Semin
    Place: Math-Mech SPbU 171
    Time:  05.2013
*)

type BigNum =

    val private sign : bool
    val private number : int list
    val private str : string
    val private length : int
    
    static member private div() = 1000000
    
    new (s : string) = 
      let signNum = if s.StartsWith("-") then BigNum.getNegativeSign() else BigNum.getPositiveSign()
      let num = if signNum then s.Substring(1) else s
      { sign = signNum; number = BigNum.parse num; str = num; length = num.Length; }
      
    new (n : int) = BigNum(n.ToString())
    new () = BigNum(0)
    new (b : BigNum) = { sign = b.sign; number = b.number; str = b.str; length = b.length; }
    
    static member zero() = BigNum()
    static member one() = BigNum(1)
    
    private new (list : int list, signNum) = 
      let s = BigNum.reparse list
      { sign = signNum; number = list; str = s; length = s.Length; }
    
    static member private getNegativeSign() = true
    static member private getPositiveSign() = false
    
    static member private parse(s : string) = 
    
      let strtoint (s : string) =
        let rec strtoint i n =
          if i = s.Length then n
          else let digit = int s.[i] - int '0'
               if digit < 0 || digit > 9 then failwith "Incorrect number given"
               else strtoint (i + 1) (n * 10 + digit)
        strtoint 0 0
        
      let len = (BigNum.div()).ToString().Length - 1
      let cut = s.Length % len
      let part = s.Substring(0, cut)
      let start = if part = "" then [] else [strtoint part]
      let rec parse i list = 
        if i >= s.Length 
          then list
        else parse (i + len) ((strtoint (s.Substring(i, len)))::list)
      parse cut start           
      
    static member private reparse(list) =
      let zeros n =
        let rec zeros i s =
          if i = n then s
          else zeros (i + 1) ("0" + s)
        zeros 0 ""
      
      let len = (BigNum.div()).ToString().Length - 1
       
      let rec reparse s = 
        function
        | [] -> s
        | hd::[] -> (hd.ToString() + s)
        | hd::tl -> 
          let lenNum = hd.ToString().Length
          let zero = zeros (len - lenNum)
          reparse (zero + hd.ToString() + s) tl
          
      reparse "" list
      
    static member private addDigit(list, n) = 
      let rec add a carry = 
        if carry = 0 then a
        else
          match a with
          | [] -> if carry > 0 then [carry] else []
          | hd::tl -> ((hd + carry) % (BigNum.div()))::(add tl ((hd + carry) / (BigNum.div())))
      add list n
      
    static member private add(a, b) = 
      let rec add a b carry =
        match (a, b) with
        | ([], b) -> BigNum.addDigit(b, carry)
        | (a, []) -> BigNum.addDigit(a, carry)
        | (hdA::tlA, hdB::tlB) -> 
          ((hdA + hdB + carry) % (BigNum.div()))::(add tlA tlB ((hdA + hdB + carry) / (BigNum.div())))
      add a b 0
      
    static member private subDigit(list, n) =
      let rec sub a carry =
        match a with
        | [] -> []
        | hd::[] -> if hd = carry then [] else [hd - carry]
        | hd::tl ->
           let diff = hd - carry
           if (diff < 0) then (diff + (BigNum.div()))::(sub tl 1)
           else (diff)::tl
      sub list n
      
    static member private sub(a, b) =
      let rec sub a b carry =
        match (a, b) with
        | ([], _) -> []
        | (a, []) -> BigNum.subDigit(a, carry)
        | (hdA::tlA, hdB::tlB) -> 
          let diff = hdA - hdB - carry
          let current = if diff < 0 then diff + (BigNum.div()) else diff
          let next = if diff < 0 then (sub tlA tlB 1)
                     else (sub tlA tlB 0)
          current::(if next = 0::[] then [] else next)
      sub a b 0
      
    static member private isZero(a : BigNum) = (a.str = "0")
      
    static member lessAbs(a : BigNum, b : BigNum) = 
      if a.length <> b.length then a.length < b.length
      else let rec less i = 
             if i = a.str.Length then false
             else if a.str.[i] = b.str.[i] then less (i + 1) else a.str.[i] < b.str.[i]
           less 0
        
    static member private equal(a : BigNum, b : BigNum) = 
      if a.sign <> b.sign || a.length <> b.length then false
      else let rec equal a b =
             match (a, b) with
                  | ([], []) -> true
                  | (hdA::tlA, hdB::tlB) -> if hdA = hdB then equal tlA tlB else false
                  | (_, _) -> false
           equal a.number b.number   
      
    static member private add(a : BigNum, b : BigNum) = 
      if a.sign = b.sign then new BigNum(BigNum.add(a.number, b.number), a.sign)
      else if BigNum.lessAbs(b, a) then new BigNum(BigNum.sub(a.number, b.number), a.sign)
           else let result = new BigNum(BigNum.sub(b.number, a.number), b.sign)
                if result.isZero() then BigNum.zero() else result
    
    static member private invert(a : BigNum) = 
      if not (a.isZero()) 
       then new BigNum(a.number, not a.sign) 
       else a
      
    static member private mult(list, n) = 
      let rec mult a carry = 
        match a with
        | [] -> if carry > 0 then [carry] else []
        | hd::tl -> 
          let t = hd * n + carry
          (t % (BigNum.div()))::(mult tl (t / (BigNum.div())))
      mult list 0
    
    static member private mult(a : int list, b : int list) =
      let rec mult (b : int list) carry = 
        match b with
        | [] -> carry
        | hdB::tlB ->
          let temp = BigNum.mult(a, hdB) 
          match (carry, temp) with
          | (hdC::tlC, hdT::tlT) ->
            let hd = hdT + hdC
            (hd % (BigNum.div()))::(mult tlB (BigNum.add(tlC, BigNum.addDigit(tlT, hd / (BigNum.div())))))
          | ([], hdT::tlT) ->
            (hdT % (BigNum.div()))::(mult tlB (BigNum.addDigit(tlT, hdT / (BigNum.div()))))
          | (_, _) -> []
          
      mult b []
         
    static member private less(a : BigNum, b : BigNum) = 
      if BigNum.equal(a, b) then false
      else if a.sign = b.sign then (BigNum.lessAbs(a, b) = a.isPositive())
           else a.isNegative()
      
    static member private greater(a : BigNum, b : BigNum) = not (BigNum.less(a, b) || BigNum.equal(a, b))
    static member private sub(a : BigNum, b : BigNum) = BigNum.add(a, BigNum.invert(b))
    static member private abs(a : BigNum) = new BigNum(a.number, BigNum.getPositiveSign())
    static member private mult(a : BigNum, n : int) = 
      if n = 0 then BigNum.zero()
      else let signN = if n < 0 then BigNum.getNegativeSign() else BigNum.getPositiveSign()
           let signProduct = if signN = a.sign then BigNum.getPositiveSign() else BigNum.getNegativeSign()
           new BigNum(BigNum.mult(a.number, abs n), signProduct)
    
    static member private mult(a : BigNum, b : BigNum) = 
      if a.isZero() || b.isZero() then BigNum.zero()
      else 
        let signProduct = if a.sign = b.sign then BigNum.getPositiveSign() else BigNum.getNegativeSign()
        new BigNum((if BigNum.less(a, b) then BigNum.mult(b.number, a.number) 
                    else BigNum.mult(a.number, b.number)), signProduct)
    
    static member private divideBySpaces(s : string) = 
      let rec divide i str = 
        if i < 0 then str
        else if i <> 0 && (s.Length - i) % 3 = 0 then divide (i - 1) (" " + s.[i].ToString() + str)
             else divide (i - 1) (s.[i].ToString() + str)
      divide (s.Length - 1) ""
    
    member a.isZero() = BigNum.isZero(a)
    member a.isNegative() = (a.sign = BigNum.getNegativeSign())
    member a.isPositive() = (a.sign = BigNum.getPositiveSign())
    member a.abs() = BigNum.abs(a)
    member a.less(b : BigNum) = BigNum.less(a, b)
    member a.greater(b : BigNum) = BigNum.greater(a, b)
    member a.equal(b : BigNum) = BigNum.equal(a, b)
    member a.add(b : BigNum) = BigNum.add(a, b)
    member a.sub(b : BigNum) = BigNum.sub(a, b)
    member a.mult(n : int) = BigNum.mult(a, n)
    member a.mult(b : BigNum) = BigNum.mult(a, b)
    
    override a.GetHashCode() = a.ToString().GetHashCode()
    override a.Equals(b) = 
        match b with
        | :? BigNum as n -> a.equal(n)
        | _ -> a.equal(new BigNum(b.ToString()))
    
    interface System.IComparable with
      member a.CompareTo(o) =
        match o with
        | :? BigNum as b -> a.CompareToObj(b)
        | _ -> a.CompareToObj(new BigNum(o.ToString()))
    
    member private a.CompareToObj(b : obj) = 
      match b with
      | :? BigNum as n -> if a.equal(n) then 0 else if a.less(n) then -1 else 1
      | _ -> a.CompareToObj(new BigNum(b.ToString()))
     
    static member op_LessThan (a : BigNum, b : BigNum) = a.CompareToObj(b) < 0
    static member op_LessThan (a : BigNum, n) = a.CompareToObj(n) < 0
    static member op_LessThan (n, a : BigNum) = a.CompareToObj(n) > 0
    static member op_GreaterThan (a : BigNum, b : BigNum) = a.CompareToObj(b) > 0
    static member op_GreaterThan (a : BigNum, n) = a.CompareToObj(n) > 0
    static member op_GreaterThan (n, a : BigNum) = a.CompareToObj(n) < 0
    static member op_Equality (a : BigNum, b : BigNum) = a.CompareToObj(b) = 0
    static member op_Equality (a : BigNum, n) = a.CompareToObj(n) = 0
    static member op_Equality (n, a : BigNum) = a.CompareToObj(n) = 0
    static member op_Inequality (a : BigNum, b : BigNum) = a.CompareToObj(b) <> 0
    static member op_Inequality (a : BigNum, n) = a.CompareToObj(n) <> 0
    static member op_Inequality (n, a : BigNum) = a.CompareToObj(n) <> 0
    static member op_LessThanOrEqual (a : BigNum, b : BigNum) = a.CompareToObj(b) <= 0
    static member op_LessThanOrEqual (a : BigNum, n) = a.CompareToObj(n) <= 0
    static member op_LessThanOrEqual (n, a : BigNum) = a.CompareToObj(n) >= 0
    static member op_GreaterThanOrEqual (a : BigNum, b : BigNum) = a.CompareToObj(b) >= 0
    static member op_GreaterThanOrEqual (a : BigNum, n) = a.CompareToObj(n) >= 0
    static member op_GreaterThanOrEqual (n, a : BigNum) = a.CompareToObj(n) <= 0
    
    static member (~-) (a : BigNum) = BigNum.invert(a)
    static member (+) (a : BigNum, b : BigNum) = a.add(b)
    static member (+) (a : BigNum, n) = a.add(new BigNum(n.ToString()))
    static member (+) (n, a : BigNum) = a + (n.ToString())
    static member (-) (a : BigNum, b : BigNum) = a.sub(b)
    static member (-) (a : BigNum, n) = a.sub(new BigNum(n.ToString()))
    static member (-) (n, a : BigNum) = a - (n.ToString())
    static member ( *) (a : BigNum, b : BigNum) = a.mult(b)
    static member ( *) (a : BigNum, n) = a.mult(new BigNum(n.ToString()))
    static member ( *) (n, a : BigNum) = a * (n.ToString())
    static member ( **) (a : BigNum, n : int) = a.power(n)
    static member Hat (a : BigNum, n : int) = a.power(n)
    
    static member factorial(n) =
      let rec factorial (a : BigNum) n =
        if n = 0 then a else factorial (a.mult(new BigNum(n))) (n - 1)
      factorial (new BigNum(1)) n
      
    member a.power(n : int) = 
      if n < 0 then failwith "Incorrect power: non-negative only"
      let rec power (res : BigNum) i =
        if i = n then res
        else power (res.mult(a)) (i + 1)
      power (BigNum.one()) 0
      
    override this.ToString() = (if this.isNegative() then "-" else "") + this.str
    member this.print = printf "%s" (this.ToString())
    member this.dividedBySpaces() = (if this.isNegative() then "-" else "") + BigNum.divideBySpaces(this.str)
    
//---------------------------------------------------------------------------------------------------------------------        
    
let rand = new System.Random()

let testOperations x y = 
    let a = new BigNum(x.ToString())
    let b = new BigNum(y.ToString())
    printf "(%s) < (%s) = (" (x.ToString()) (y.ToString())
    printf " %b" (a < b)
    printfn ") %s" (if (x < y) = (a < b) then "" else "FAIL")
    
    printf "(%s) > (%s) = (" (x.ToString()) (y.ToString())
    printf " %b" (a > b)
    printfn ") %s" (if (x > y) = (a > b) then "" else "FAIL")
    
    printf "(%s) == (%s) = (" (x.ToString()) (y.ToString()) 
    printf " %b" (a = b)
    printfn ") %s" (if (x = y) = (a = b) then "" else "FAIL")
    
    printf "(%s) + (%s) = (" (x.ToString()) (y.ToString())
    (a + b).print
    printfn ") %s" (if (x + y).ToString() = (a + b).ToString() then "" else "FAIL")
    
    printf "(%s) - (%s) = (" (x.ToString()) (y.ToString())
    (a - b).print
    printfn ") %s" (if (x - y).ToString() = (a - b).ToString() then "" else "FAIL")
    
    printf "(%s) * (%s) = (" (x.ToString()) (y.ToString())
    (a * b).print
    printfn ") %s" (if (x * y).ToString() = (a * b).ToString() then "" else "FAIL")
    printfn "------------------------------\n"
    
printfn "Some tests:"
    
let mutable count = 0
let amountTests = 100000

for i in 1..amountTests do
    let maxDiff = 10001
    let x = rand.Next(2 * maxDiff) - maxDiff
    let y = rand.Next(2 * maxDiff) - maxDiff
    let a = new BigNum(x.ToString())
    let b = new BigNum(y.ToString())
    let test = (x < y) = (a < b)
               && (x > y) = (a > b)
               && (x = y) = (a = b)
               && (x <> y) = (a <> b)
               && (x <= y) = (a <= b)
               && (x >= y) = (a >= b)
               && (x + y).ToString() = (a + b).ToString() 
               && (x - y).ToString() = (a - b).ToString()
               && (x * y).ToString() = (a * b).ToString()
    if test then count <- count + 1
    else testOperations x y
   
printfn "tests(%i/%i) = %i%%"  count amountTests (count * 100 / amountTests)

let two = new BigNum(2)
let power = 5000
printf "\n%s^%i = " (two.ToString()) power
(two.power(power)).print
printfn "\n"
  
let num = 1000
printf "%i! = " num
(BigNum.factorial(num)).print
printfn "\n"  
  
printfn "\n%A" "happy end (I hope)"
