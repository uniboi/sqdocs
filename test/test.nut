global function TestFnc
global function FunnyTest
//globalize_all_functions

global struct FunnyStruct {
	int bitfield = 0x001,
}

// debug
void function Test1(){}

/*
	multiple doc comments are ignored
*/
/*
	# Test

	multiline doc comment

	line 2

	`test1`

	```
	var l = TestFnc() // null
	printt( l )
	```

	<script>alert("epic haxxor")</script>
*/
function TestFnc(string s = "test", void functionref() c = void function(){print("smth")})
{
	int c,g
}

FunnyStruct function FunnyTest()
{
	FunnyStruct f
	return f
}