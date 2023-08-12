expression(Value) --> number(Value).
expression(Value) --> number(X), [+], expression(V),	{Value is X+V}.
expression(Value) --> number(X), [-], expression(V),		{Value is X-V}.
expression(Value) --> number(X), [*], expression(V),	{Value is X*V}.
expression(Value) --> 	number(X), [/], expression(V),	{V=\=0, Value is X/V}.
expression(Value) --> left_parenthesis, expression(Value), right_parenthesis.
left_parenthesis --> ['('].
right_parenthesis --> [')'].
number(X) --> digit(X).
number(Value) --> digit(X), number(Y),{numberofdigits(Y,N), Value is X*2^N+Y}.
digit(0) --> [0].
digit(1) --> [1].


numberofdigits(Y,1) :- Z is Y/2, Z<1,!.
numberofdigits(Y,N) :- 
	Z is (Y - mod(Y,2))/2,
	numberofdigits(Z,N1), 
	N is N1+1,!.

dec2Bin(0,V,_,V).
dec2Bin(N,V,Counter,Val):-
    Reminder is N mod 2,
    N1 is N//2,
    V1 is V + Reminder*(10^Counter),
    Counter1 is Counter + 1,
    dec2Bin(N1,V1,Counter1,Val).

convert(N,V):-
    N > -1,
    dec2Bin(N,0,0,V).

