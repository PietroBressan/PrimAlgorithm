%%%% -*- Mode: Prolog -*-

% README.txt

% --------------------------------------------------------------------

% Nome File: mst.pl


% ------------------------------------------------------------------------


Progetto realizzato da: 

Samuele Campanella [851781]
Pietro Bressan [852260]


% ------------------------------------------------------------------------

L'obiettivo del progetto è l'implementazione di un codice scritto in
linguaggio Prolog che, dato un grafo non diretto e connesso e presi 
due nodi A e B, permetta di trovare il percorso con peso minimo che 
li collega attraverso l'algoritmo di Prim.

% ------------------------------------------------------------------------
Per comprendere il progetto, è necessario avere una conoscenza per
quanto riguarda il linguaggio Prolog, l'algoritmo di Prim, grafi e
heap.
% ------------------------------------------------------------------------

Lista e spiegazione di predicati utilizzati nell'implementazione
del codice per quanto riguarda la gestione dei grafi:


:- new_graph(G). Questo predicato inserisce un nuovo grafo nella
base-dati Prolog.


:- delete_graph(G).
Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati
Prolog.


:- new_vertex(G, V).
Aggiunge il vertice V nella base-dati Prolog. N.B. si richiede che
il predicato che rappresenta i vertici, da aggiungere alla
base-dati Prolog, sia vertex(G, V). 


:- graph_vertices (G, Vs).
Questo predicato è vero quando Vs è una lista contenente
tutti i vertici di G.


:- list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog una lista
dei vertici del grafo G.


:- new_arc(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog. N.B. è richiesto
che il predicato che rappresenta gli archi, da aggiungere alla
base-dati Prolog, sia arc(G, U, V, Weight).


:- graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi
presenti in G.


:- vertex_neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una lista
contenente gli archi, arc(G, N, V, W), che portano ai vertici N
immediatamente raggiungibili da V.


:- adjs(G, V, Vs).
Questo predicato è vero quando V è un vertice di G e Vs è una lista
contenente i vertici, vertex(G, V), ad esso adiacenti;
si noti che in un grafo non diretto si devono inserire nella lista
Vs tutti i vertici adiacenti.
A questa implementazione sono state aggiunte due righe di codice
che permettono il corretto funzionamento del predicato
mst_prim(G, Source).


:- adjs_no_confirmed(Cs1, Cs2, Cs).
Questo predicato non era richiesto nella consegna ed è vero quando
Cs è una lista contentente gli elementi di Cs1 meno quelli di Cs2,
in particolare Cs non contiene elementi già asseriti con il
predicato confirmed(G, HV).


:- list_arcs(G).
Questo predicato stampa alla console dell’interprete Prolog una
lista degli archi del grafo G (è il simmetrico di list_vertices/1).


:- list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog una lista
dei vertici e degli archi del grafo G.


:- read_graph(G, FileName).
Questo predicato legge un “grafo” G, da un file FileName e lo
inserisce nel data base di Prolog. Ogni riga contiene 3 elementi
separati da un carattere di tabulazione (in altre parole è un
“tab separated value” file, o un “CSV” file con separatore il
carattere di tabulazione). Il tipo del file deve essere “.csv”.


:- insert(G, Ls).
Questo predicato non era richiesto nella consegna ed è vero
quando all'interno della base di conoscenza vengono inseriti
i vertici e gli archi nel grafo G estrapolando le
informazioni dalla lista Ls.


:- write_graph(G, FileName).
:- write_graph(G, FileName, Type).
Questo predicato è vero quando G viene scritto sul file FileName
secondo il valore dell’argomento Type. Type può essere graph o edges.
Se Type è graph, allora G è un termine che identifica un grafo nella
base di dati Prolog; In FileName saranno scritti gli archi
del grafo secondo il formato descitto per read_graph/2. Se Type è
edges, allora G è una lista di archi, ognuno dei quali viene stampato
su FileName, sempre secondo il formato descritto per read_graph/2.

% ------------------------------------------------------------------------

Lista e spiegazione di predicati utilizzati nell'implementazione
del codice per quanto riguarda l'algoritmo di Prim:


:- vertex_key(G, V, K).
Questo predicato è vero quando V è un vertice di G e, durante e dopo
l’esecuzione dell’algoritmo di Prim, 
contiene il peso minimo di un arco che connette V nell’albero minimo;
se questo arco non esiste (ed all’inizio dell’esecuzione) allora K è
inf (cfr., [CLR+09] 23.2).
Questo predicato va dichiarato dynamic.


:- vertex_previous(G, V, U).
Questo predicato è vero quando V ed U sono vertici di G e, durante e
dopo l’esecuzione dell’algoritmo di Prim, il vertice U è il vertice
“genitore” (“precedente”, o “parent”) di V nel minimum spanning tree
(cfr., [CLR+09] 23.2).
Questo predicato va dichiarato dynamic.


:- mst_prim(G, Source).
Questo predicato ha successo con un effetto collaterale.
Dopo la sua prova, la base-dati Prolog ha al suo interno i predicati
vertex_key(G, V, k) per ogni V appartenente a G; la base-dati Prolog
contiene anche i predicati vertex_previous(G, V, U) per ogni V,
ottenuti durante le iterazioni dell’algoritmo di Prim. Naturalmente i
predicati vertex_key(G, V, K) e vertex_previous(G, V, U) devono essere
corretti rispetto alla soluzione del problema MST.


:- prim(G, Source).
Questo predicato non era richiesto nella consegna ed è
l'implementazione di mst_prim(G, Source), scritta in questo
predicato per questione di ordine.


:- recursive_prim(_, []).
:- recursive_prim(G, Vs).
Questo predicato non era richiesto nella consegna ed è vero quando
viene estratta la testa dell'heap G, aggiunta all'MST e i suoi adiacenti
vengono aggiunti all'heap G.


:- vertex_compare(_, _, []).
:- vertex_compare(G, V, As).
Questo predicato non era richiesto nella consegna ed è vero quando
V è un vertice appartenente a G ed esiste un arco che lo connette
a gli elementi della lista As.


:- populate(_, _, []).
:- populate(G, Source, Vs).
Questo predicato non era richiesto nella consegna e
aggiunge alla base di conoscenza i predicati vertex_key e
vertex_previous con valori di default per ogni elemento di Vs.


:- mst_get(G, Source, [])
:- mst_get(G, Source, PreorderTree).
Questo predicato è vero quando PreorderTree è una lista degli archi
del MST ordinata secondo un attraversamento preorder dello stesso,
fatta rispetto al peso dell’arco.


:- create_list(G, V, PreorderTree).
Questo predicato non era richiesto nella consegna ed è l'
implementazione di mst_get, scritta in questo predicato
per questione di ordine.


:- recursive_create_list(_, _, [], PreorderTree).
:- recursive_create_list(G, V, Ls, PreorderTree).
Questo predicato non era richiesto nella consegna ed è vero
quando PreorderTree è una lista ordinata secondo il sorting
specificato nella consegna e i cui elementi sono la lista Ls.


:- get_vertex(G, V, Ls).
Questo predicato non era richiesto nella consegna ed è vero quando
Ls è una lista di archi che connettono V a qualsiasi altro vertice
in G.

% ------------------------------------------------------------------------

Lista e spiegazione di predicati utilizzati nell'implementazione
del codice per quanto riguarda il MinHeap:


Un MINHEAP è una struttura dati che prevede le sequenti operazioni:
NEWHEAP, INSERT, HEAD, EXTRACT, MODIFYKEY.


:- new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.


:- delete_heap(H).
Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati
Prolog.


:- heap_head(H, K, V).
Il predicato head/3 è vero quando l’elemento dello heap H con 
chiave minima K è V.


:- heap_has_size(H, S).
Questo predicato è vero quanto S è la dimensione corrente dello heap. 


:- heap_empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.


:- heap_not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.


:- heap_insert(H, K, V).
Il predicato insert/3 è vero quando l’elemento V è inserito nello
heap H con chiave K.
Naturalmente, lo heap H dovrà essere ristrutturato in modo da
mantenere la proprietà che heap_head(H, HK, HV) sia vero per HK minimo
e che la "heap property" sia mantenuta ad ogni nodo dello heap.


:- heap_extract(H, K, V).
Il predicato extract/3 è vero quando la coppia K, V con K minima,
è rimossa dallo heap H.
Naturalmente, lo heap H dovrà essere ristrutturato in modo da
mantenere la proprietà che heap_head(H, HK, HV) sia vero per HK
minimo e che la "heap property" sia mantenuta ad ogni nodo dello heap.


:- min_pair([(K1, V1) | Xs], (K, V)).
:- min_pair([], (K, V), (K, V)).
:- min_pair([(K2, V2) | Xs], (K1, _), (K, V)).
:- min_pair([(_,_) | Xs], (K1, V1), (K, V)).
Questo predicato non era richiesto nella consegna ed è vero quando
la coppia (K, V) è la coppia minore contenuta in una lista passata
come primo parametro.


:- buildheap(_, 0).
:- buildheap(H,F). 
Questo predicato non è richiesto nella consegna ed è vero quando H è
un heap di dimensione F che mantiene le proprietà di un MinHeap
attraverso heapify.


:- remove_node(H, P, K, V).
Questo predicato non è richiesto nella consegna ed è vero quando
il vertice V di chiave K alla posizione P viene rimosso da H.


:- min_heapify(H, I).
Questo predicato non è richiesto nella consegna ed è vero quando H è
un heap che mantiene le proprietà di un MinHeap a partire dalla
posizione I fino alla radice.
E' implementato 3 volte per differenziare i vari casi in cui il nodo
considerato abbia uno, più figli oppure è un nodo foglia.


:- modify_key(H, NewKey, OldKey, V).
Il predicato modify_key/4 è vero quando la chiave OldKey (associata
al valore V) è sostituita da NewKey. Naturalmente, lo heap H dovrà
essere ristrutturato in modo da mantenere la proprietà che
heap_head(H, HK, HV) sia vero per HK minimo e che la “heap property”
sia mantenuta ad ogni nodo dello heap.


:- list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog lo
stato interno dello heap.


:- ordering(_, 0).
:- ordering(H, S).
Questo predicato non era richiesto nella consegna ed è vero quando
H ha elementi ordinati per la stampa sulla console Prolog.

% ------------------------------------------------------------------------

% end of file - mst.pl