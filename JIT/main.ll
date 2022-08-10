declare void @greetings(...)

define dso_local i32 @main(
	i32		%argc,
	i8**		%argv
	)
{
call void (...) @greetings()

ret i32 0
}

