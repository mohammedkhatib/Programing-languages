:-use_module(library(clpfd)).

diamonds.
spades.
clubs.
hearts.
n(X):- X in 2..14 .
suit(X):- X=diamonds;X=spades;X=clubs;X=hearts.
%card(X,Y):- suit(Y),n(X).

card(2,diamonds).
card(3,diamonds).
card(4,diamonds).
card(5,diamonds).
card(6,diamonds).
card(7,diamonds).
card(8,diamonds).
card(9,diamonds).
card(10,diamonds).
card(11,diamonds).
card(12,diamonds).
card(13,diamonds).
card(14,diamonds).
card(2,spades).
card(3,spades).
card(4,spades).
card(5,spades).
card(6,spades).
card(7,spades).
card(8,spades).
card(9,spades).
card(10,spades).
card(11,spades).
card(12,spades).
card(13,spades).
card(14,spades).
card(2,clubs).
card(3,clubs).
card(4,clubs).
card(5,clubs).
card(6,clubs).
card(7,clubs).
card(8,clubs).
card(9,clubs).
card(10,clubs).
card(11,clubs).
card(12,clubs).
card(13,clubs).
card(14,clubs).
card(2,hearts).
card(3,hearts).
card(4,hearts).
card(5,hearts).
card(6,hearts).
card(7,hearts).
card(8,hearts).
card(9,hearts).
card(10,hearts).
card(11,hearts).
card(12,hearts).
card(13,hearts).
card(14,hearts).

%section1 q1

su(clubs,X):- X=hearts;X=spades;X=diamonds.
su(hearts,X):- X=spades;X=diamonds.
su(spades,diamonds).


legal(X,Y,A,B):- n(X),n(A),suit(Y),suit(B) .
legalC(card(X,Y)):-n(X),suit(Y).

lowesth([],Z,Z):- legalC(Z),!.
lowesth([card(X,Y)|T],G,card(Z,M)):- legal(X,Y,Z,M),X#<Z,lowesth(T,G,card(X,Y)),!.
lowesth([card(X,Y)|T],G,card(Z,M)):- legal(X,Y,Z,M),X#>Z,lowesth(T,G,card(Z,M)),!.
lowesth([card(X,Y)|T],G,card(Z,M)):- legal(X,Y,Z,M),su(Y,M),lowesth(T,G,card(X,Y)),!.
lowesth([_|T],G,card(Z,M)):- lowesth(T,G,card(Z,M)).

lowest([],card(_,_)).
lowest([X|T],Y):- lowesth(T,Y,X).


%section2 q1
filterh(_,[],Z,Z).
filterh(card(N,S),[card(X,Y)|T],G,M):- legal(N,S,X,Y),N#<X,filterh(card(N,S),T,G,[card(X,Y)|M]),!.

filterh(card(N,S),[card(X,Y)|T],G,M):- legal(N,S,X,Y),
N#>X,filterh(card(N,S),T,G,M),!.

filterh(card(N,S),[card(X,Y)|T],G,M):- legal(N,S,X,Y),su(S,Y),filterh(card(N,S),T,G,[card(X,Y)|M]),!.

filterh(card(N,S),[_|T],G,M):- filterh(card(N,S),T,G,M).

filter(_,[],[]).
filter(C,Y,G):- filterh(C,Y,G,[]).


%section3 q1

min(D,K,A,B,R):- lowest(D,A),lowest(K,B),min2(A,B,R).

min2(card(X,_),card(A,_),R):- X#<A,R#=1,!.
min2(card(X,_),card(A,_),R):- X#>A,R#=2,! .
min2(card(X,Y),card(A,B),R):- X#=A,su(Y,B),R#=1 ,!.
min2(card(X,Y),card(A,B),R):- X#=A,su(B,Y),R#=2 ,!.
min2(_,_,R):- R#=0 .

winnerH(X,Y,C,R,D,K):- filter(C,X,D),filter(C,Y,K),D=[],K=[],R#=0,!.
winnerH(X,Y,C,R,D,K):- filter(C,X,D),filter(C,Y,K),K=[],R#=1,! .
winnerH(X,Y,C,R,D,K):- filter(C,X,D),filter(C,Y,K),D=[],R#=2,! .
winnerH(X,Y,C,R,D,K):- filter(C,X,D),filter(C,Y,K), min(D,K,_,_,R).


winner([],[],_,R):- R #=0,!.
winner(X,Y,C,R):- winnerH(X,Y,C,R,_,_).

%q2
replace([|P], 0, Q, [Q|P]).
replace([P|Z],Id1, Q,[P|M]):-  (Id1 > -1),Id2 is (Id1-1), replace(Z,Id2,Q, M),!,!.
replace(S, , , S).
turing(Begin, Li, tape(V,Id), tape(N,Id)):- nth0(Id,V,E), member(action(Begin,,E,EN,h),Li), replace(V,Id,EN,N), !,!.
turing(Begin, Li, tape(V,Id), X):- nth0(Id,V,E), member(action(Begin,Stop,E,EN,r),Li), replace(V,Id,EN,NV), J is Id+1,turing(Stop, Li, tape(NV,J), X),!,!.
turing(Begin, Li, tape(V,Id), X):- nth0(Id,V,E), member(action(Begin,Stop,E,EN,l),Li), replace(V,Id,EN,NV), J is Id-1,turing(Stop, Li, tape(NV,J), X),!,!.

%q3

help([]).
help([_]).
help([X1,X2|Xs]):-X1>=X2,help([X2|Xs]).

change(0,[]).
change(N,[X|Xs]):-member(X,[1,5,10,50,100]),N1 is N-X,N1>=0,change(N1,Xs),help([X|Xs]).