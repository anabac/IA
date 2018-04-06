elem_count(_, [], 0).
elem_count(X, [X|Rs], Xn) :- elem_count(X, Rs, Xm), Xn is Xm + 1.
elem_count(X, [Y|Rs], Xn) :- X \= Y, elem_count(X, Rs, Xn).