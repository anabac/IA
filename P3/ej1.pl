pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [L|_]) :- pertenece_m(X, L).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).