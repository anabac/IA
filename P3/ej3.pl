insert([X-P], [], [X-P]).
insert([X-P], [X1-P1|Rs], [X-P|[X1-P1|Rs]]) :- P =< P1.
insert([X-P], [X1-P1|Rs], [X1-P1|L]) :- P > P1, insert([X-P], Rs, L).