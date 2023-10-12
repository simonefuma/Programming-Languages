module type StackAbs = 
	sig

		type 'a stack

		exception Empty_stack

		val create : unit -> 'a stack
		val push : 'a stack -> 'a -> 'a stack
		val pop : 'a stack -> 'a stack * 'a
		val top : 'a stack -> 'a
		val is_empty : 'a stack -> bool

	end;;