let argc = Array.length Sys.argv in
if argc > 1 then Mexpr.Run.run Sys.argv.(1) else Mexpr.Run.repl ()
