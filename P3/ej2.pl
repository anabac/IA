concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).

invierte([], []).
invierte([X|L], L1) :- invierte(L, L2), concatena(L2, [X], L1).