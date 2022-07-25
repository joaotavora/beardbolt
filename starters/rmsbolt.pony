// pony beardbolt starter file

// Passing '--debug' is recommended to pony because without it LOC hints are optimized out

// Local Variables:
// beardbolt-command: "ponyc --debug"
// beardbolt-disassemble: nil
// End:

actor Main
	new create(env: Env) =>
		var a: U8 = 1 + 1
		if is_rms(a) != 0 then
			env.out.print(a.string())
		end

	fun ref is_rms(a: U8): I32 =>
		match a
			| 'R' => 1
			| 'M' => 2
			| 'S' => 3
			else     0
		end
