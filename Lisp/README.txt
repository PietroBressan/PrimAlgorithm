; README.txt

; nome file: mst.lisp

Progetto realizzato da:
 
Samuele Campanella [851781]
Pietro Bressan [852260]


L'obiettivo del progetto è l'implementazione di un codice scritto in
linguaggio Lisp che, dato un grafo non diretto e connesso e presi 
due nodi A e B, permetta di trovare il percorso con peso minimo che 
li collega attraverso l'algoritmo di Prim.


Per comprendere il progetto, è necessario avere una conoscenza per
quanto riguarda il linguaggio Prolog, l'algoritmo di Prim, grafi e
heap.



Nota:
Le funzioni non richieste nella specifica di base
sono contrassegnate con ++ sia nel ReadMe che nel
file .lisp


-----Gestione grafo------

- is-graph *graph-id* -> *graph-id* oppure NIL 

Questa funzione controlla se l'id del grafo passato come
parametro corrisponde ad un grafo creato.
Se si ritorna l'id stesso del grafo, altrimenti ritorna NIL.


- new-graph *graph-id* -> *graph-id*

Questa funzione riceve in ingresso l'id di un grafo.
Se il grafo non è ancora stato creato, e quindi non ha 
un'entry nella hash-table *graphs*, lo aggiunge e ritorna l'id.
Se già esiste allora ritorna solo l'id.


- delete-graph *graph-id* -> NIL

Questa funzione esegue la cancellazione di un grafo.
In particolar modo elimina le entry nelle hash-table *graphs*,
*arcs* e *vertecies* relative a quel grafo.
Ciò significa che insieme al grafo sono eliminate anche tutte le 
informazioni relative ad esso come archi e vertici.


- new-vertex *graph-id vertex-id* -> *vertex-rep*

La funzione controlla sia che vertex-id abbia un valore atomico 
(o un simbolo non NIL o un valore numerico) sia che già
non esista una entry per tale vertex-id in correlazione al graph-id.
In ogni caso ritorna una rappresentazione del vertice, cioè ritorna la
corrispettiva entry nella hash-table *vertices*.


- graph-vertices *graph-id* -> *vertex-rep-list*

La funzione scorre tutta la hash-table *vertices* in modo da trovare
i vertici relativi al grafo corrispondente al graph-id passato come
parametro.
Ritorna poi la lista con tutti i vertici.


- new-arc *graph-id vertex-id vertex-id2 &optional weight* -> *arc-rep*

Questa funzione aggiunge un arco nella hash-table *arcs*.
In particolare controlla che il grafo corrispondente al graph-id esista,
aggiunge i due vertici passati come parametro nella hash-table *vertices*
(qualora questi già esistano la hash-table rimarrà invariata) e infine 
esegue un controllo sul parametro opzionale weight.
Se il peso è nullo, cioè non è stato specificato, o è negativo allora
si inizializza in automatico a 1, mentre se ha un valore numerico non nullo
allora tale valore è utilizzato nella creazione dell'arco.

Si ricorda inoltre che gli archi sono rappresentati da liste 
nella forma (arc graph-id vertex-id vertex-id2 weight).


- graph-arcs *graph-id* -> *arc-rep-list*

Graph-arcs ritorna una lista contenente tutti gli archi relativi al grafo
corrispondente al graph-id passato come parametro.


- graph-vertex-neighbors *graph-id vertex-id* -> *arc-rep-list*

Questa funzione ritorna una lista di archi nella forma
(arc graph-id vertex-id neighbor weight), cioè una lista di 
archi che connettono direttamente il vertice corrispondente a 
vertex-id ai suoi vertici adiacenti.
Inoltre la presenza di cappi ovvero di archi nella forma
(arc graph-id vertex-id vertex-id weight) non è prevista all'interno
della lista *arc-rep-list*.


++ find-arcs *graph-id vertex1 vertex2* -> *arc-rep-list*

Find-arcs è una funzione non richiesta di base dalla specifica ma utile
nel flusso di esecuzione del programma perchè ritorna una lista
di archi nella forma (arc graph-id vertex-id vertex-id2 weight).


- graph-vertex-adjacent *graph-id vetex-id* -> *vertex-rep-list*

Questa funzione ritorna la lista di vertici adiacenti al vertice 
corrispondente al vertex-id.
Tutti i vertici presenti nella lista sono unici, cioè non sono
ammessi valori doppi.


++ check-in-list *value values* -> T oppure NIL

Questa funzione, non richiesta nella specifica di base, prende 
come parametri una lista values e un elemento value.
Qualora value si trovi all'interno della lista viene ritornato il valore T, 
e il valore NIL altrimenti.


++ find-alternative-routes *vertex-id adjs graph-id* -> *arc-rep-list*

Questa funzione non richiesta dalla specifica base del progetto 
ritorna una lista contenente gli archi (arc graph-id adj vertex-id weight)
dove adj è un elemento contenuto nella lista adjs.
Questi archi sono ritornati per ogni elemento di adjs.


- graph-print  *graph-id* -> NIL

Questa funzione stampa a console le informazioni riguardanti il grafo 
passato come parametro. 
Nello specifico vengono stampati prima i suoi nodi e dopo i suoi archi.
Il valore di ritorno è NIL.


-----Gestione heap------


- new-heap *heap-id &optional capacity* -> *heap-rep*

Questa funzione crea nella hash-table *heaps* una nuova entry nella
forma (heap heap-id size array).
La size inizialmente è 0 e l'array ha dimensione pari al valore
passato come capacity; si noti che qualora il valore fosse 
omesso di defaut viene inizializzato a 42.
La entry è poi ritornata come valore della funzione.
Se l'heap dovesse essere già presente nella hash-table allora si ritorna 
solo la relativa entry.


- heap-delete *heap-id* -> Boolean

Questa funzione lo heap dalla hash-table *heaps*, cioè ne 
cancella corrispettiva entry per poi ritornare il valore T.
Se lo heap non dovesse essere presente in *heaps* allora il valore
di ritorno è NIL.


- heap-empty *heap-id* -> Boolean

Questa funzione ritorna T se lo heap è vuoto e NIL altrimenti.


- heap-not-empty *heap-id* -> Boolean

Questa funziona ritorna T se lo heap contiene almeno un elemento
e NIL altrimenti.


- heap-head *heap-id* -> *(K V)*

Questa funziona ritorna l'elemento in testa allo heap
ovvero in prima posizione nell'array corrispettivo.
La testa sarà nella forma 
(K V) dove K è un arco che porta al vertice V.


- heap-insert *heap-id key value* -> Boolean

Questa funzione inserisce nello heap un nuovo elemento (key value).
Fatto ciò viene richiamata l'operazione di heapify
così da mantenere le proprietà della struttura dati.
Inoltre ad ogni insert si controlla sia che il valore dell'elemento
da aggiungere non sia già stato visitato sia che lo 
heap non sia pieno; se così dovesse essere lo heap viene
espanso dinamicamente.
Viene dunque ritornato un booleano che indica se
l'elemento è stato inserito o meno.


++ copy-heap *heap1 heap2 size index* -> NIL

Questa funzione non prevista nella specifica base copia
gli elementi di heap1 nello heap2, cioè copia gli elementi
del primo array nel secondo.
Per fare ciò si passa anche la dimensione (size) del primo heap
e l'indice dal quale partire per copiare gli elementi 
(nel progetto questo è sempre 0).
Il valore di ritorno è NIL.


++ heap-multiple-insert *heap-id arcs* -> NIL

Questa funzione non richiesta dalla specifica base prende come parametro
una lista di archi e lo heap dove vanno inseriti.
Procede poi inserendo uno ad uno tutti gli archi nello heap.


- heap-extract *heap-id* -> *(K V)*

Questa funzione ritorna l'elemento, cioè la coppia (K V), in testa allo
heap e lo rimuove.
In questa fase viene eseguita un'operazione di top-down heapify per
mantenere le proprietà della struttura dati.


- heap-modify-key *heap-id new-key old-key V* -> Boolean

Questa funzione cerca l'elemento old-key dentro all'heap passato
come parametro e lo modifica con new-key.
Ritorna dunque un booleano che indica se l'operazione è 
avvenuta o meno.
Per fare questo si appoggia alla funzione complementare search-and-modify.


++ serch-and-modify *heap size new-key old-key index* -> Boolean

Questa funzione di supplemento cerca l'elemento old-key all'interno 
dello heap (cioè dell'array) passato come parametro a partire
da index.
Una volta trovato l'elemento lo sostituisce con new-key ma solo
se new-key rappresenta un arco di peso minore.
Viene poi richiamata l'operazione di heapify per 
mantenere le proprietà dello heap.
Il valore di ritorno è un boolean che indica qualora la modifica sia
avvenuta o meno.


- heap-print *heap-id* -> Boolean

La funzione heap-print stampa lo stato dello heap passato come 
parametro.
Finita la stampa il valore T è ritornato, NIL qualora lo heap
passato come parametro non esista nella hash-table *heaps*.


++ print-array *heap size index* -> NIL

Questa funzione non richiesta nella specifica base riceve in ingresso 
uno heap (un array) e stampa ogni suo elemento secondo il formato
*Posizione* : *Chiave*, *Valore*, partendo dalla posizione 
index e senza chiaramente sforare la size dello heap.


++ heapify *heap size index* -> NIL

Questa funzione di appoggio permette di mantenere allo heap
passato come parametro le proprietà della struttura dati.
Per fare ciò heapify risale lo heap, riordinandolo,
fino ad arrivare alla radice.


++ topdown-heapify *heap size inde* -> NIL

Il funzionamento di questa funzione, anch'essa di appoggio, 
è lo stesso della funzione di heapify.
La differenza è che però qui viene usato un approccio top-down
cioè partendo dalla radice fino ad arrivare ad un nodo foglia.
Questa funzione è richiamata dalla funzione heap-extract.


------------PRIM----------------

- mst-vertex-key *graph-id vertex-id* -> K

Questa funzione prende in ingresso l'id di un grafo e di un
vertice appartenente ad esso.
Ne ritorna dunque il peso minimo dell'arco che connette
il vertice al resto del MST.
Se questo vertice non era mai stato visitato prima allora
il peso (K) viene aggiunto insieme al vertice nella forma
(K vertex-id) nella hash-table *vertex-keys*.


- mst-previous *graph-id vertex-id* -> V

Questa funzione recupera il vertice V precedente
al vertice corrispondente a vertex-id nell'MST.
Tale vertice precedente è anche il valore di ritorno
della funzione.
Qualora vertex-id non fosse mai stato visitato prima allora 
l'informazione vertex-id e parent vertex-id è salvata
nella hash-table *previous* nella forma
(graph-id vertex-id previous).


- mst-prim *graph-id source* -> NIL

Questa funzione esegue l'algoritmo di Prim sul grafo relativo
a graph-id partendo da un suo vertice source.
Durante l'esecuzione vengono aggiunte le entry nelle hash-table 
*vertex-keys* e *previous* che serviranno poi per mantere traccia 
dell'MST.
Crea anche una struttura dati heap per lo svolgimento dell'algoritmo
e, una volta terminato, lo heap è cancellato.


- mst-get *graph-id* -> *preorder-mst*

Questa funzione ritorna una lista degli archi che compongono 
l'MST dopo averli ordinati secondo una visita preoder dell'MST
fatta a partire dai pesi degli archi.
 

++ get-arcs *graph-id keys previous* -> *arc-rep-list*

Questa funzione non richiesta dalla specifica base ritorna gli archi 
presenti nel grafo e che fanno parte dell'MST.
Per fare ciò fa richiamo della funzione build-arc-info.


++ build-arc-info *graph-id key previous* -> *arc-rep-list*

Questa funzione non richiesta dalla specifica prende una key
nella forma (K Vertice) e ritorna, dopo un confronto, con
gli elementi di previous gli archi nella forma
(arc graph-id vertex prev K) dove prev è un elemento 
di previous.

; end of file ReadMeLisp.txt








 