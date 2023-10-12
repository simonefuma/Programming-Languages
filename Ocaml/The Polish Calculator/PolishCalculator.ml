module PolishCalculator (Stack : StackAbs) = 
	struct
		
		type expression = Value of float | Binary of expression * operator * expression
										and operator = Plus | Min | Mul | Div | Pow 

		exception Not_valid_expression

		let expr_of_string str = 
		  let operator_parser op =
				match op with
				| "+"  -> Plus
				| "-"  -> Min
				| "*"  -> Mul
				| "/"  -> Div
				| "**" -> Pow
				| _    -> raise Not_valid_expression
			in let rec expr_of_string stack lst =
				match lst with 
				|	[]   -> snd (Stack.pop stack)
				| h::t -> match float_of_string_opt h with
									| Some n -> expr_of_string (Stack.push stack (Value n)) t
									| None   -> let op = operator_parser h in
															let c1 = Stack.pop stack in
														  let c2 = Stack.pop (fst c1) in
														  expr_of_string (Stack.push (fst c2) (Binary((snd c2), op, (snd c1)))) t

			in expr_of_string (Stack.create()) (String.split_on_char ' ' str)

		let rec eval expr =
			match expr with
			| Value n              -> n
			| Binary(op1, op, op2) -> let op1 = eval op1 in
																let op2 = eval op2 in
																match op with
																| Plus -> op1 +. op2
																| Min  -> op1 -. op2
																| Mul  -> op1 *. op2
																| Div  -> op1 /. op2
																| Pow  -> op1 ** op2
													 

	end;;