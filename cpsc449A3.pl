% Name: Nam Nguyen Vu 
% UCID: 30154892
% Tutorial 3
% TA: Zhang, Si


%Problem 1
%For this question, I have 2 base cases in which the first 
%input is empty or the second input is empty
%Otherwise, in the recursive step, I will seperate both 2 lists 
%into the head and the tail then the riffle of XS and YS will 
%go to R1 and I will add Y to R1 using R2 = [Y|R1] then I will 
%add X to R2 using R = [X|R2] and R will be returned as the 
%final answer.

riffle(+,+,+).
riffle(+,+,-).
riffle(-,+,+).
riffle(+,-,+).
riffle([],B,B).
riffle(A,[],A).
riffle([X|XS],[Y|YS],R) :- riffle(XS,YS,R1), R2 = [Y|R1], R = [X|R2].


%Problem 2
%For this function, the goal is to keep the term T in the new 
%list.
%For the base case, if the list is empty then an empty list will
%be returned regardless of the term being passed.
%If the term appear in the first element, then we run recursively 
%on the rest of the list with the head being preserved in the 
%output list.
%If the term doesn't appear in the first element, then we run 
%recursively on the rest of the list with the head not being 
%preserved in the output list.

list_only(+,+,+).
list_only(+,+,-).
list_only(-,-,-).
list_only([],_,[]).
list_only([X|XS],T,[X|R]) :- X = T, list_only(XS,T,R).
list_only([X|XS],T,R) :- X \= T, list_only(XS,T,R).

%Problem 3
%In this question, we are trying to preserve the negative terms
%of the list and get rid of the negative term.
%The base case will be if you input an empty list then you will
%get an empty list as the output.
%Otherwise, if the first term is less than 0, then you run 
%recursively on the tail of the list without preserving the head.
%If the first term is greater than or equal to 0, then you run 
%recursively on the tail of the list while preserving the head.
%The reason is that 0 is neither positive nor negative so 0 is not
%negative.

neg_only(+,+).
neg_only(+,-).
neg_only([],[]).
neg_only([X|XS],R) :- X < 0, neg_only(XS,R).
neg_only([X|XS],[X|R]) :- X >= 0, neg_only(XS,R).


%Problem 4
%In this question, I build a my_concat function which help to 
%concatinate 2 lists.
%
%With the gtree_list function, the base case will be if the input
%is an empty list then the output is an empty list.
%If the input is a leaf then I will return the value of the leaf
%as the output.
%
%If the input is a list of leaf and the tail is empty then I will
%find the value of the leaf and put it into a list before 
%returning as if the input is a list then I should return a list
%as the output.
%
%If the input is a list of leaf and the tail is not empty then I 
%will find the value of the leaf and recursively find the list 
%of the value of its tail and put the value of the head leaf into
%a list then concatinate with the list of value of the tail
%put it into a list before returning.
%
%I will follow the same approach if the input is a gnode instead
%of the leaf.
%I added the exclamation mark at line 115, 129 so that when 
%the function execute with the type gtree_list(-,+). It will end 
%up halting

% ** I modified my code to be without the concatination function, 
%as in the previous version of my code, the way I format all 
%result to be in 1 single answer using "my_concat([Result1],
%Result2,Result)." and then return Result make it impossible 
%for the code to work in the mode "gtree_list(-,+)." as all 
%the answer are in the same type.

%Now, I modified my code to be so that the result will be in 
%the type it need to be so it will works with the mode 
%"gtree_list(-,+)."


leaf(_).
gnode([]).
gnode([leaf(_)|T]) :- gnode(T).
gnode([gnode(X)|T]) :- gnode(X), gnode(T).

gtree_list(+,+).
gtree_list(-,+).
gtree_list(+,-).

gtree_list(leaf(X),X).

gtree_list(gnode([]),[]).

gtree_list(gnode([leaf(X)]),Result1) :- 
    gtree_list(leaf(X),Result), Result1 = [Result], !.

gtree_list(gnode([leaf(X)|T]),[Result1|Result2]) :- 
    (   T \= []; Result2 \= []),
    gtree_list(leaf(X),Result1),
    gtree_list(gnode(T),Result2).

gtree_list(gnode([gnode(X)|T]),Result) :- 
    (   T = []; Result = [_|[]]),
    gtree_list(gnode(X),Result1), Result = [Result1].

gtree_list(gnode([gnode(X)|T]),[Result1|Result2]) :- 
    (   T \= []; Result2 \= []),
    gtree_list(gnode(X),Result1), 
    gtree_list(gnode(T),Result2), !.

%Problem 5
%For this problem, I need to convert an integer input to natural
%type. 
%If the input is less than 0, then I return false. The method I 
%used is based on question 6 in lab 9 of Bradley Li (Tutorial 2)
%The reason that the first if statement work is that if X is 
%greater than 0 then it continue executing to false but then the
%exclamation prevents the code to trace back so it returns false.
%If the input is not ness than 0 then the program execute the 
%other statements. 
%If the input is than 0, then I return R is nat_zero.
%If the input is greater than 0, then I recursively call int_nat
%(X1,R1) in which X1 is X - 1 and R is the successor of R1.

succ(succ(X)) :- succ(X).
succ(nat_zero).
nat_zero.

int_nat(+,+).
int_nat(+,-).
int_nat(X,_) :- X < 0,!,false.
int_nat(X,R) :- X is 0, R = nat_zero.
int_nat(X,R) :- X > 0, X1 is X - 1, int_nat(X1,R1), R = succ(R1).



%Problem 6
%For this question it's kind of messy as I make it works for 
%bin_nat(+,-) but then the reverse use case requires lots of 
%subfunction

bin(bin(X),bin(Y,Z)):-
    bin(X),
    bin(Y,Z).
bin(bin(X),bin(Y)):-
    bin(X),
    bin(Y).
    bin(zero).
    bin(one).

%The first function is bin_int, this function is used to convert
%binary to int. bin(zero) will be 0, bin(one) will be 1. 
%Otherwise, I recursively call on BinA and BinB to find its 
%binary representation. I partition using bin(BinA, BinB) as 
%binary here is kind of a tree structure instead of set so I 
%can't use [A|B] to partition.

bin_int(bin(zero), 0) :- !.
bin_int(bin(one), 1).
bin_int(bin(BinA, BinB), Int) :-
    bin_int(BinA, A),
    bin_int(BinB, B),
    bin_length(bin(BinA, BinB), L),
    Int is A * 2^(L-1) + B.

%The second function is bin_nat. This function is used to 
%monitor the function work for bin_nat(+,-), bin_nat(-,+) and
%bin_nat(+,+). If it's in the form bin_nat(-,+), then I need 
%to call function nat_bin (nat to bin). 
%Otherwise, I use function bin_int then int_nat to convert
%from bin to nat.

bin_nat(X, R) :- 
    (var(X), nonvar(R) -> nat_bin(R,X);
    bin_int(X,R1), int_nat(R1,R)).

%This is nat_bin function. This function make use of nat_int 
%and int_bin to convert from nat_bin.
nat_bin(R,X) :- nat_int(R,X1), int_bin(X1,X).

%For int_bin, I use pattern matching, if input is 0, then I 
%return bin(zero). If input is 1, then I return bin(one). 
%Otherwise, I make use of int_binFake function so that the 
%output will not have extra bin().
%For instance, int_bin(5,X). will give X =  bin((bin((bin(one),
%bin(zero))),bin(one))) instead of
%X = bin(bin(bin(bin(bin(one)),bin(bin(zero)))),bin(bin(one))).
int_bin(0,bin(zero)).
int_bin(1,bin(one)).
int_bin(X,bin(R)) :- int_binFake(X, R).


int_binFake(X1,R) :- X1 > 1,
    ModAns is X1 mod 2, 
    ModAns1 is X1 div 2, 
    int_bin(ModAns1, BinAFake),
    int_bin(ModAns, BinBFake),
    R = (BinAFake,BinBFake).


%This is the nat_int function, this is the reverse of int_nat
%in question 5.
%If the input is nat_zero, I return 0.
%Otherwise, each time I remove a succ() layer, then I add 1 to
%R and recursively call it until X reach nat_zero and the 
%function halt.
nat_int(nat_zero, 0).
nat_int(succ(X), R) :- nat_int(X, R1), R is R1 + 1.


%This is the bin_length function. In this function, I use the 
%partition way I use in bin_int because "bin" here is in tree 
%type, not list.
%At the end, I add 2 tree side together to return the output.
bin_length(bin(zero), 1).
bin_length(bin(one), 1).
bin_length(bin(BinA, BinB), Length) :-
    bin_length(BinA, Length1),
    bin_length(BinB, Length2),
    Length is Length1 + Length2.
