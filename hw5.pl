% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(C, P) :- parent(P, C).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(P) :- parent(P, _),
               female(P).

isFather(P) :- parent(P, _),
               male(P).

% 3. Define a predicate `grandparent/2`.
grandparent(G, X) :- parent(G, P),
                     parent(P, X).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(A, B) :- parent(P, A), parent(P, B), \+ (A = B).

% 5. Define two predicates `brother/2` and `sister/2`.
brother(A, B) :- male(A), sibling(A, B).

sister(A, B) :- female(A), sibling(A, B).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(A, B):- married(A, X), sibling(X, B) ; sibling(A, Y), married(Y, B).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(A, B):- sister(A, X), parent(X, B) ; parent(X, B), siblingInLaw(A, X), female(A).
uncle(A, B):- brother(A, X), parent(X, B); parent(X, B), siblingInLaw(A, X), male(A).

% 8. Define the predicate `cousin/2`.
cousin(A, B):- parent(X, A), sibling(X, Y), parent(Y, B); parent(X, A), sibling(X, Y), married(Y, Z), parent(Z, B).

% 9. Define the predicate `ancestor/2`.
ancestor(A, B):- parent(A, B) ; parent(A, X), ancestor(X, B).

% Extra credit: Define the predicate `related/2`.
related(A, B) :- ancestor(A, B); 
                 sibling(A, X), ancestor(X, B);
                 married(A, X), ancestor(X, B);
                 cousin(A, B).


%%
% Part 2. Implementing a stack language
%%


% 1. Define the predicate `cmd/3`.


% 2. Define the predicate `prog/3`.
%

