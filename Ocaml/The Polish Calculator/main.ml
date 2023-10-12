module Polish = PolishCalculator(StackList);;

let print str r = 
  print_string(str ^ " = "^ string_of_float r);;


let str1 = "((8 + 3) * (12 - 4)) / 2";; 
let str2 = "5 + (4 * (10 - (7 + 3)) / 2)";; 
let str3 = "(3 + 2) * ((4 - 2) ** 2) - 1";; 
let str4 = "2 * (3 + 5) ** ((4 / 2) + 1)";; 
let str5 = "((6 + 4) * 3 - (2 + 1)) / (5 - 1)";; 

let r1 = Polish.eval (Polish.expr_of_string "8 3 + 12 4 - * 2 /");;
let r2 = Polish.eval (Polish.expr_of_string "5 4 10 7 3 + - * 2 / +");;
let r3 = Polish.eval (Polish.expr_of_string "3 2 + 4 2 - 2 ** * 1 -");;
let r4 = Polish.eval (Polish.expr_of_string "2 3 5 + 4 2 / 1 + ** *");;
let r5 = Polish.eval (Polish.expr_of_string "6 4 + 3 * 2 1 + - 5 1 - /");;

print str1 r1;;
print str2 r2;;
print str3 r3;;
print str4 r4;;
print str5 r5;;
