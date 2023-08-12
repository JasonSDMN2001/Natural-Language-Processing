expression(Value) --> number(Value).
expression(Value) --> number(X), [+], expression(V),	{Value is X+V}.
expression(Value) --> number(X), [-], expression(V),		{Value is X-V}.
expression(Value) --> number(X), [*], expression(V),	{Value is X*V}.
expression(Value) --> 	number(X), [/], expression(V),	{V=\=0, Value is X/V}.
expression(Value) --> left_parenthesis, expression(Value), right_parenthesis.
left_parenthesis --> ['('].
right_parenthesis --> [')'].
number(X) --> digit(X).
number(Value) --> digit(X), number(Y),{numberofdigits(Y,N), Value is X*10^N+Y}.
digit(0) --> [0].
digit(1) --> [1].
digit(2) --> [2].
digit(3) --> [3].
digit(4) --> [4].
digit(5) --> [5].
digit(6) --> [6].
digit(7) --> [7].
digit(8) --> [8].
digit(9) --> [9].

numberofdigits(Y,1) :- Z is Y/10, Z<1.
numberofdigits(Y,N) :- 
	Z is (Y - mod(Y,10))/10,
	numberofdigits(Z,N1), 
	N is N1+1.


dec2Bin(0,V,_,V).
dec2Bin(N,V,Counter,Val):-
    Reminder is N mod 2,
    N1 is N//2,
    V1 is V + Reminder*(10^Counter),
    Counter1 is Counter + 1,
    dec2Bin(N1,V1,Counter1,Val).

convert(N,V):-
    N > -1,
    dec2Bin(N,0,0,V),
    writeln(V).

tobin(0,0).
tobin(S,X) :- X > 0 , 
               X1 is X // 2 , 
               tobin(S1,X1),  
               S0 is X mod 2 , 
               S is S0 + S1 * 10 .
cbd(Binary, Decimal) :-
    number_codes(Binary, Codes),
    number_codes(Decimal, [0'0, 0'b| Codes]).
binary_expression(X,+,Y,Ans):-cbd(X,A),cbd(Y,B),expression(V,[A,+,B],[]),convert(V,Ans).
binary_expression(X,-,Y,Ans):-cbd(X,A),cbd(Y,B),expression(V,[A,-,B],[]),convert(V,Ans).
binary_expression(X,*,Y,Ans):-cbd(X,A),cbd(Y,B),expression(V,[A,*,B],[]),convert(V,Ans).
binary_expression(X,/,Y,Ans):-cbd(X,A),cbd(Y,B),expression(V,[A,/,B],[]),convert(V,Ans).







:- use_module(library(clpfd)). %for #=, which gives value on the right 
%hand side which otherwise would be imposible because X and Y dont have
%a value before we run the program. simply using "is" doesnt work


binary_addition(Xs, Ys, As):-
        phrase(binary_addition_(Xs, Ys, 0), As).


/*The SWI-Prolog implementation of phrase/3 verifies that the List and
 Rest arguments are unbound, bound to the empty list or a list cons cell.
 Other values raise a type error.*/

binary_addition_([], [], 0)     --> [0].
binary_addition_([], [], 1)     --> [1].
binary_addition_([X|Xs], [], C) --> binary_addition_([X|Xs], [C], 0).
binary_addition_([], [Y|Ys], C) --> binary_addition_([C], [Y|Ys], 0).
binary_addition_([X|Xs], [Y|Ys], C0) -->
        { [X,Y] ins 0..1,
          Sum #= X + Y + C0 }, %at first time,the carry bit is 0
        sum_carry(Sum, C),
        binary_addition_(Xs, Ys, C).

sum_carry(0, 0) --> [0].
sum_carry(1, 0) --> [1].
sum_carry(2, 1) --> [0].
reverse([], Y, R) :-
    R = Y.
reverse([H|T] , Y, R) :-
    reverse(T, [H|Y], R).


binary_sub(Xs, Ys, As):-
        phrase(binary_sub_(Xs, Ys), As).
binary_sub_([],[])-->[].
binary_sub_([X|Xs], []) --> [].
binary_sub_([], [Y|Ys]) --> [].
binary_sub_([X|Xs], [Y|Ys]) -->
        { [X,Y] ins 0..1,
          Sub #= X + Y}, %για να μην βγαινει αρνητικο
        sub_carry(Sub),
        binary_sub_(Xs, Ys).

sub_carry(0)--> [0].
sub_carry(1)--> [1].
sub_carry(2)--> [0].

expression(Xs,+, Ys,Ans):-reverse(Xs,[],X),reverse(Ys,[],Y),binary_addition(X, Y, As),reverse(As,[],Ans). 		
expression(Xs,-, Ys,Ans):-reverse(Xs,[],X),reverse(Ys,[],Y),binary_sub(X, Y, As),reverse(As,[],Ans). 		



