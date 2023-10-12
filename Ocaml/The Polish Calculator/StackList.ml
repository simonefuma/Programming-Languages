module StackList : StackAbs = 
	struct
		
		type 'a stack = { lst : 'a list }

		exception Empty_stack

		let create () = { lst = [] }

		let push stack value = { lst = value::stack.lst }

		let pop stack =
			match stack.lst with
			|	[]   -> raise Empty_stack
			| h::t -> ({ lst = t }, h)

		let top stack =
			match stack.lst with
			|	[]   -> raise Empty_stack
			| h::t -> h

		let is_empty stack = List.length stack.lst = 0

	end;;