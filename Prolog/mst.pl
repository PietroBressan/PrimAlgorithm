%%%% -*- Mode: Prolog -*-
% mst.pl
% prolog

% MINIMUM SPANNING TREE

% Samuele Campanella [851781]
% Pietro Bressan [852260]

library(csv).

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic confirmed/2.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic heap_head/3.




% test preso dalla pagina wikipedia
test_1(G) :-
    new_graph(G),
    new_vertex(G, source),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, final),
    new_arc(G, a, b, 6),
    new_arc(G, source, a, 2),
    new_arc(G, source, d, 8),
    new_arc(G, a, c, 2),
    new_arc(G, d, c, 2),
    new_arc(G, d, e, 3),
    new_arc(G, c, e, 9),
    new_arc(G, e, final, 1),
    new_arc(G, b, final, 5).

%test inventato a 9 nodi
test_2(G) :-
    new_graph(G),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, f),
    new_vertex(G, g),
    new_vertex(G, h),
    new_vertex(G, i),
    new_arc(G, a, b, 1),
    new_arc(G, b, c, 4),
    new_arc(G, d, e, 2),
    new_arc(G, e, f, 5),
    new_arc(G, g, h, 2),
    new_arc(G, h, i, 8),
    new_arc(G, a, d, 3),
    new_arc(G, d, g, 9),
    new_arc(G, b, e, 4),
    new_arc(G, e, h, 9),
    new_arc(G, c, f, 6),
    new_arc(G, f, i, 50),
    new_arc(G, b, d, 12),
    new_arc(G, d, h, 15),
    new_arc(G, h, f, 32),
    new_arc(G, f, b, 7).

% test per funzionamento predicati heap

% insert/extract/modify_key
test_heap(H) :-
    new_heap(H),
    heap_insert(H, 12, a),
    heap_insert(H, 7, b),
    heap_insert(H, 50, c),
    heap_insert(H, 12, d),
    heap_insert(H, 4, e),
    heap_insert(H, 6, f),
    list_heap(H),
    delete_heap(H).


% new graph

new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

% delete graph

delete_graph(G) :-
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    retract(graph(G)), !.

% new vertex

new_vertex(G, V) :-
    vertex(G, V), !.

new_vertex(G, V) :-
    assert(vertex(G, V)), !.

% graph vertices

graph_vertices(G, Vs) :-
    findall(V, (vertex(G, V)), Vs), !.

% list vertices

list_vertices(G) :-
    listing(vertex(G, _)), !.

% new arc

new_arc(G, U, V, Weight) :-
    arc(G, U, V, Weight), !.

new_arc(G, U, V, Weight) :-
    new_vertex(G, U),
    new_vertex(G, V),
    assert(arc(G, U, V, Weight)), !.

% graph arcs

graph_arcs(G, Es) :-
    findall(arc, arc(G, _, _, _), Es), !.

% vertex neighbors

vertex_neighbors(G, V, Ns) :-
    findall(arc, arc(G, V, _, _), Ns), !.

% adjs

adjs(G, V, Vs) :-
    findall(A1, arc(G, V, A1, _), As1),
    findall(A2, arc(G, A2, V, _), As2),
    append(As1, As2, Vs),
    findall(HV, confirmed(G, HV), Cs),
    adjs_no_confirmed(Cs, _, Vs).

% adjs no confirmed

adjs_no_confirmed(Cs, [], Cs) :- !.

adjs_no_confirmed(Cs1, Cs2, Cs) :-
    Cs2 = [L | Ls],
    member(L, Cs1),
    delete(Cs1, L, Gs),
    adjs_no_confirmed(Gs, Ls, Cs), !.

adjs_no_confirmed(Cs1, Cs2, Cs) :-
    Cs2 = [_ | Ls],
     adjs_no_confirmed(Cs1, Ls, Cs), !.

% list arcs

list_arcs(G) :-
    listing(arc(G, _, _, _)).

% list graph

list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_arcs(G), !.

% read graph

read_graph(G, FileName) :-
    graph(G),
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName, List, [separator(0'\t)]),
    insert(G, List).

read_graph(G, FileName) :-
    new_graph(G),
    csv_read_file(FileName, List, [separator(0'\t)]),
    insert(G, List).

% insert

insert(_, []) :- !.

insert(G, [L | Ls]) :-
    functor(L, row, 3),
    arg(1, L, Arg1),
    new_vertex(G, Arg1),
    arg(2, L, Arg2),
    new_vertex(G, Arg2),
    arg(3, L, Arg3),
    new_arc(G, Arg1, Arg2, Arg3),
    insert(G, Ls),
    !.

% write graph

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

write_graph(G, FileName, Type) :-
    Type = graph,
    graph(G),
    graph_arcs(G, Ls),
    csv_write_file(FileName, Ls, [separator(0'\t)]).

write_graph(G, FileName, Type) :-
    is_list(G),
    Type = edges, !,
    csv_write_file(FileName, G, [separator(0'\t)]).


% MST

% mst prim

mst_prim(G, Source) :-
    graph(G),
    vertex(G, Source),
    prim(G, Source), !.

% prim

prim(G, Source) :-
    new_heap(G),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    retractall(confirmed(G, _)),
    graph_vertices(G, Vs),
    populate(G, Source, Vs),
    recursive_prim(G, Vs),
    delete_heap(G).

% recursive prim

recursive_prim(_, []) :- !.

recursive_prim(G, Vs) :-
    heap_extract(G, _, HV),
    delete(Vs, HV, Ts),
    assert(confirmed(G, HV)),
    adjs(G, HV, As),
    vertex_compare(G, HV, As),
    recursive_prim(G, Ts), !.

% vertex compare

vertex_compare(_, _, []) :- !.

% caso 1: abbiamo trovato un arco che connette V
% a U ma il suo peso è più grande di quello
% già esistente. Non c'è bisogno di fare
% un replace.

vertex_compare(G, V, As) :-
    As = [U | Ts],
    arc(G, V, U, W),
    vertex_key(G, V, UK),
    UK @=< W,
    vertex_compare(G, V, Ts), !.

% caso 2: abbiamo trovato un arco che connette V
% a U e il suo peso è più piccolo di quello già
% esistente. Facciamo un replace.

vertex_compare(G, V, As) :-
    As = [U | Ts],
    arc(G, V, U, W),
    vertex_key(G, V, UK),
    UK @> W,
    retract(vertex_key(G, V, _)),
    assert(vertex_key(G, V, W)),
    retract(vertex_previous(G, U, _)),
    assert(vertex_previous(G, U, V)),
    modify_key(G, W, UK, V),
    vertex_compare(G, V, Ts), !.

% populate

populate(_, _, []) :- !.

populate(G, Source, Vs) :-
    Vs = [Source | Ts],
    assert(vertex_key(G, Source, 0)),
    heap_insert(G, 0, Source),
    populate(G, Source, Ts), !.

populate(G, Source, Vs) :-
    Vs = [T | Ts],
    assert(vertex_key(G, T, inf)),
    assert(vertex_previous(G, T, not_defined)),
    heap_insert(G, inf, T),
    populate(G, Source, Ts), !.

% mst get

mst_get(G, Source, PreorderTree) :-
    graph(G),
    vertex(G, Source),
    create_list(G, Source, PreorderTree).

% create list

create_list(G, V, PreorderTree) :-
    get_vertex(G, V, Ls1),
    sort(3, @=<, Ls1, Ls2),
    sort(4, @=<, Ls2, Ls),
    recursive_create_list(G, V, Ls, PreorderTree).


% recursive create list

recursive_create_list(_, _, [], PreorderTree) :-
    PreorderTree = [], !.

recursive_create_list(G, V, Ls, PreorderTree) :-
    Ls = [arc(G, V, U, W) | Os],
    get_vertex(G, U, Ts1),
    sort(3, @=<, Ts1, Ts2),
    sort(4, @=<, Ts2, Ts3),
    recursive_create_list(G, U, Ts3, Ts4),
    append([arc(G, V, U, W)], Ts4, Ls1),
    recursive_create_list(G, V, Os, Ls2),
    append(Ls1, Ls2, PreorderTree), !.


% get vertex

get_vertex(G, V, Ls) :-
    findall(arc(G, V, U, W), (arc(G, V, U, W), vertex_previous(G, U, V)), L1),
    findall(arc(G, V, U, W), (arc(G, U, V, W), vertex_previous(G, U, V)), L2),
    append(L1, L2, Ls).


% MIN HEAP

% new heap

new_heap(H) :-
    heap(H, _), !.

new_heap(H) :-
    assert(heap(H, 0)), !.

% delete heap

delete_heap(H) :-
    retract(heap(H, _)),
    retractall(heap_entry(H, _, _, _)), !.

% heap has size

heap_has_size(H, S) :-
    heap(H, S), !.

% heap empty

heap_empty(H) :-
    heap(H, 0).

% heap not empty

heap_not_empty(H) :-
    heap(H, _),
    heap_has_size(H, S),
    S > 0, !.

% heap insert

heap_insert(H, K, V) :- heap_entry(H, _, K, V), !.

% caso1: stiamo inserendo un elemento
% con K minore di quella precedentemente assegnata
% a quest'ultimo. Facciamo un replace

heap_insert(H, K, V) :-
    heap_entry(H, P, K1, V),
    K @< K1,
    retract(heap_entry(H, P, K1, V)),
    assert(heap_entry(H, P, K, V)).

% caso2: inseriamo elemento nell'heap che ancora
% non era comparso.

heap_insert(H, K, V) :-
    heap(H, S),
    P is S + 1,
    retract(heap(H, S)),
    assert(heap_entry(H, P, K, V)),
    assert(heap(H, P)).

% heap extract

heap_extract(H, K, V) :-
    findall((K2, V2), heap_entry(H, _, K2, V2), Ps),
    min_pair(Ps, (K,V)),
    heap_entry(H, P, K, V),
    remove_node(H, P, K, V),
    heap(H, S),
    P is S - 1,
    retract(heap(H, S)),
    assert(heap(H, P)),
    F is P / 2,
    buildheap(H, F).

% min pair

min_pair([(K1, V1) | Xs], (K, V)) :-
    min_pair(Xs, (K1, V1), (K, V)).

min_pair([], (K, V), (K, V)).

min_pair([(K2, V2) | Xs], (K1, _), (K, V)) :-
    K2 @< K1, !,
    min_pair(Xs, (K2, V2), (K, V)).

min_pair([(_,_) | Xs], (K1, V1), (K, V)) :-
    min_pair(Xs, (K1, V1), (K, V)).

% buildheap

buildheap(_, 0).

buildheap(H, F) :-
    min_heapify(H, F),
    P is F - 1,
    buildheap(H, P).

% remove node (il parent del nodo da rimuovere ha due figli)

remove_node(H, P, K, V) :-
    heap_entry(H, P, K, V),
    Isx is P * 2,
    Idx is (P * 2) + 1,
    heap_entry(H, Isx, Ksx, Vsx),
    heap_entry(H, Idx, Kdx, _),
    Ksx @< Kdx,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, P, Ksx, Vsx)),
    remove_node(H, Isx, Ksx, Vsx).

% remove node (il parent del nodo da rimuovere ha un figlio a sinistra)

remove_node(H, _, K, V) :-
    heap_entry(H, P, K, V),
    Isx is P * 2,
    heap_entry(H, Isx, Ksx, Vsx),
    retract(heap_entry(H, P, K, V)),
    retract(heap_entry(H, Isx, Ksx, Vsx)),
    assert(heap_entry(H, P, Ksx, Vsx)).

% remove node (il parent del nodo da rimuovere ha un figlio a destra)

remove_node(H, _, K, V) :-
    heap_entry(H, P, K, V),
    Idx is (P * 2) + 1,
    heap_entry(H, Idx, Kdx, Vdx),
    retract(heap_entry(H, P, K, V)),
    retract(heap_entry(H, Idx, Kdx, Vdx)),
    assert(heap_entry(H, P, Kdx, Vdx)).

% remove node (il nodo da rimuovere è foglia)

remove_node(H, _, K, V) :-
    retract(heap_entry(H, _, K, V)).

% min heapify

min_heapify(_, 0).

min_heapify(H, I) :-
    Isx is I * 2,
    Idx is (I * 2) + 1,
    heap_entry(H, Isx, Ksx, Vsx),
    heap_entry(H, Idx, Kdx, _),
    heap_entry(H, I, K, V),
    Ksx @< K,
    Ksx @< Kdx,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Isx, Ksx, Vsx)),
    assert(heap_entry(H, Isx, K, V)),
    assert(heap_entry(H, I, Ksx, Vsx)),
    I2 is I / 2,
    min_heapify(H, I2).

min_heapify(H, I) :-
    Isx is I * 2,
    Idx is (I * 2) + 1,
    heap_entry(H, Isx, Ksx, _),
    heap_entry(H, Idx, Kdx, Vdx),
    heap_entry(H, I, K, V),
    Kdx @< K,
    Kdx @< Ksx,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Idx, Kdx, Vdx)),
    assert(heap_entry(H, Idx, K, V)),
    assert(heap_entry(H, I, Kdx, Vdx)),
    I2 is I / 2,
    min_heapify(H, I2).

min_heapify(H, I) :-
    Isx is I * 2,
    heap_entry(H, Isx, Ksx, Vsx),
    heap_entry(H, I, K, V),
    Ksx @< K,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Isx, Ksx, Vsx)),
    assert(heap_entry(H, Isx, K, V)),
    assert(heap_entry(H, I, Ksx, Vsx)),
    I2 is I / 2,
    min_heapify(H, I2).

% modify key

modify_key(H, NewKey, OldKey, V) :-
    heap(H, _),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    buildheap(H, P).

% list heap

list_heap(H) :-
    heap(H, _),
    heap_size(H, S),
    ordering(H, S),
    listing(heap_entry(H, _, _, _)), !.

% ordering

ordering(_, 0) :- !.

ordering(H, S) :-
    retract(heap_entry(H, S, K, V)),
    assert(heap_entry(H, S, K, V)),
    Size is S - 1,
    ordering(H, Size), !.

% end of file - mst.pl

















