;mst.lisp
;Lisp

;Samuele Campanella [851781]
;Pietro Bressan [852260]


(defparameter *vertices* (make-hash-table :test 'equal))
(defparameter *graphs* (make-hash-table :test 'equal))
(defparameter *arcs* (make-hash-table :test 'equal))
(defparameter *visited* (make-hash-table :test 'equal))
(defparameter *vertex-keys* (make-hash-table :test 'equal))
(defparameter *previous* (make-hash-table :test 'equal))
(defparameter *heaps* (make-hash-table :test 'equal))

;-----------------grafo----------------------

;is-graph
(defun is-graph (graph-id) (gethash graph-id *graphs*))

;new-graph
(defun new-graph (graph-id) 
  (cond
   ((null (is-graph graph-id))
    (setf (gethash graph-id *graphs*) graph-id)))
  (gethash graph-id *graphs*))

;delete-graph
(defun delete-graph (graph-id) 
  (remhash graph-id *graphs*)
  ;rimuoviamo ogni arco e vertice associato al grafo
  (maphash (lambda (key value) (cond
                                ((eql graph-id (second key))
                                 (remhash key *vertices*)))) *vertices*)
  (maphash (lambda (key value) (cond
                                ((eql graph-id (second key))
                                 (remhash key *arcs*)))) *arcs*))

;new-vertex                              
(defun new-vertex (graph-id vertex-id) 
  (cond
   ;controlo che il vertice da inserire sia un valore atomico
   ((and (atom vertex-id) (is-graph graph-id)
         (null (gethash (list graph-id vertex-id) *vertices*))) 
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
          (list 'vertex graph-id vertex-id)))))

;graph-vertices   
(defun graph-vertices (graph-id)
  (let ((a (list)))
    (maphash (lambda (key value)
               (cond ((eql graph-id (second key)) 
                       (push (third key) a))))  *vertices*) (reverse a)))

;new-arc
(defun new-arc (graph-id vertex-id vertex-id2 &optional weight) 
  (cond 
   ;controllo che il grafo e i vertici siano validi
   ((is-graph graph-id)
    ;inserisco i vertici nel mio grafo, se sono
    ;già inseriti allora new-vertex non farà nulla.
    (new-vertex graph-id vertex-id)
    (new-vertex graph-id vertex-id2)
    (cond
     ;controllo se il peso passato è nullo
     ((or (null weight)
          (<= weight 0)) 
      (setf (gethash (list 'arc graph-id vertex-id vertex-id2 1) *arcs*)
                          (list 'arc graph-id vertex-id vertex-id2 1)))
     (T
      (setf (gethash (list 'arc graph-id vertex-id vertex-id2 weight) *arcs*) 
              (list 'arc graph-id vertex-id vertex-id2 weight)))))))

;graph-arcs                    
(defun graph-arcs (graph-id)
  (let ((a ()))
    (maphash (lambda (key value)
               (cond ((eql graph-id (second key)) 
                       (push (cdr (cdr key)) a))))  *arcs*) (reverse a)))    

;graph-vertex-neighbors
(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((a ()))
    (maphash (lambda (key value)
               ;controllo tra gli arcs esistenti quali si 
               ;connettono a vertex-id. 
               ;Escludiamo però gli archi che rappresentano
               ;cappi e che sono quindi nella forma
               ;(arc graph-id vertex-id vertex-id weight).
               (cond ((and (eql graph-id (second key))
                           (eql vertex-id (third key))
                           (not (eql vertex-id (fourth key))))
                      (push key a)))) *arcs*) (reverse a)))

;find-arcs++
(defun find-arcs (graph-id vertex-id vertex-id2)
  (let 
      ((a ()))
    (maphash (lambda (key value)
               ;recuperiamo gli archi nella forma
               ;(arc graph-id vertex1 vertex2 weight)
               (cond ((and (eql graph-id (second key))
                           (eql vertex-id (third key))
                           (eql vertex-id2 (fourth key)))
                      (push 
                       (list 'arc graph-id vertex-id2 vertex-id
                             (fifth key)) a))))
             *arcs*) (reverse a)))

;graph-vertex-adjacent                                                                
(defun graph-vertex-adjacent (graph-id vertex-id)
  (let* ((adjacent ()))
    (maphash (lambda (key value)
               (cond
                ((and (eql graph-id (second key)))
                 (cond
                  ;inseriamo tutti i vertici adiacenti stando attenti
                  ;a non inserire valori doppi
                  ;(qualora due vertici siano connessi tra loro da più archi)
                  ((and (eql vertex-id (third key))
                        (null
                         (check-in-list 
                          (list 'vertex graph-id (fourth key)) adjacent)))
                   (push
                    (list 'vertex graph-id (fourth key)) adjacent))
                  ((and (eql vertex-id (fourth key))
                        (null
                         (check-in-list 
                          (list 'vertex graph-id (third key)) adjacent)))
                   (push (list 'vertex graph-id (third key)) adjacent))))))
             *arcs*) adjacent))


;check-in-list++
(defun check-in-list (value values)
  (cond
   ((null values) nil)
   ((and (eql (second value) (second (car values)))
         (eql (third value) (third (car values)))) T)
   (T (check-in-list value (cdr values)))))

;find-alternative-routes++
(defun find-alternative-routes (vertex-id adjs graph-id)
  (cond
   ((null adjs) nil)
   (T (append (find-arcs graph-id (third (car adjs)) vertex-id)
              (find-alternative-routes vertex-id (cdr adjs) graph-id)))))

;graph-print
(defun graph-print (graph-id) 
  (format t "Grafo ~S:~%" graph-id)
  (maphash (lambda (key value)
             (cond
              ((eql graph-id (second key)) 
               (format t "~S~%" (third key))))) *vertices*)
  (maphash (lambda (key value)
             (cond
              ((eql graph-id (second key)) 
               (format t "~S, ~S, ~S~%" (third key)
                       (fourth key) (fifth key))))) *arcs*))

;-----------heap-----------------------

;new-heap
(defun new-heap (heap-id &optional (capacity 42))
  (cond 
   ((null (gethash heap-id *heaps*))
    (setf (gethash heap-id *heaps*) 
          (list 'heap heap-id 0 (make-array capacity))))
   (T (gethash heap-id *heaps*))))

;heap-delete
(defun heap-delete (heap-id) (remhash heap-id *heaps*))

;heap-empty
(defun heap-empty (heap-id)
  (cond
   ((eql 0 (third (gethash heap-id *heaps*))) T)))

;heap-not-empty
(defun heap-not-empty (heap-id) (not (heap-empty heap-id)))

;heap-head
(defun heap-head (heap-id)
  (aref (fourth (gethash heap-id *heaps*)) 0))

;heap-insert
(defun heap-insert (heap-id key value)
  (cond
   ;controllo che i parametri passati siano validi,
   ;quindi che
   ; - il quinto capo della chiave 
   ; (che si ricorda essere un arco) sia effettivamente un numero
   ; - la chiave abbia lunghezza 5 (e quindi segue la struttura di un arco)
   ; - il valore passato, cioè il vertice da raggiungere, non sia già 
   ;   stato visitato.
   ;   Se così fosse non ci importa aggiungerlo di nuovo nello heap.
   ((and (numberp (fifth key))
         (= (length key) 5)
         (null (gethash (list heap-id value) *visited*)))
    (let 
        ;recupero l'array che contiene lo heap e la sua size.
         ((heap (fourth (gethash heap-id *heaps*)))
          (size (third (gethash heap-id *heaps*))))
       
      (cond
        ;l'array è pieno
       ((= (array-dimension heap 0) size)
        ;creiamo un nuovo heap più grande
        (heap-delete heap-id)
        (new-heap heap-id (+ size 10))
        (let*
            ((new-heap (fourth (gethash heap-id *heaps*))))
           ;copiamo i valori del vecchio heap e li mettiamo in quello nuovo
          (copy-heap heap new-heap size 0)
          (setf (third (gethash heap-id *heaps*)) size)
          ;aggiungiamo poi il valore nel nuovo array espanso e 
          ;richiamiamo l'heapify
          (setf (aref new-heap size) (list key value))
          (heapify new-heap (+ size 1) (floor (+ size 1) 2))
          (setf (gethash heap-id *heaps*)
                (list 'heap heap-id (+ size 1) new-heap))
          T))
       (T
        ;se l'array non è pieno allora basta aggiungere il valore
        ;in fondo all'heap e poi richiamare l'heapify
        (setf (aref heap size) (list key value))
        (heapify heap (+ size 1) (floor (+ size 1) 2))
        (setf (gethash heap-id *heaps*)
              (list 'heap heap-id (+ size 1) heap))
        T))))))

;copy-heap++
(defun copy-heap (heap1 heap2 size1 index)
  (cond
   ;scorro il primo array copiando i suoi valori dentro ad un altro array
   ((< index size1)
    (setf (aref heap2 index) (aref heap1 index))
    (copy-heap heap1 heap2 size1 (+ index 1)))))

;heap-multiple-insert++
(defun heap-multiple-insert (heap-id arcs)
  (cond 
   ((null arcs) nil)
   ;aggiungo di volta in volta un elemento della lista passato
   ;come parametro tramite heap-insert.
   (T 
    (heap-insert heap-id (car arcs) (fourth (car arcs)))
    ;richiamo ricorsivamente heap-multiple-insert
    ;per aggiungere gli elementi del resto della lista
    (heap-multiple-insert heap-id (cdr arcs)))))


;heap-extract
(defun heap-extract (heap-id)
  (cond 
   ((> (third (gethash heap-id *heaps*)) 0)
    (let* 
      ((heap (fourth (gethash heap-id *heaps*)))
       (size (third (gethash heap-id *heaps*)))
       (last (aref heap (- size 1)))
       (head (aref heap 0)))
      ;all'extract di un elemento, recuperiamo l'ultimo elemento nello heap,
      ;lo posizioniamo in testa e richiamiamo l'heapify con un approccio
      ;top-down.
    (setf (aref heap 0) last)
    (topdown-heapify heap size 1)
    (setf (aref heap (- size 1)) nil)
    (setf (gethash heap-id *heaps*)
          (list 'heap heap-id (- size 1) heap))
    head)))) 

;heap-modify-key
(defun heap-modify-key (heap-id new-key old-key value)
                  (cond
                   ((not (null (gethash heap-id *heaps*)))
                    (let*
                        ;recupero lo heap
                        ((heap (fourth (gethash heap-id *heaps*)))
                         (size (third (gethash heap-id *heaps*))))
                    (cond
                     ;controllo che l'old-key sia presente nello
                     ;heap e nel caso lo modifico
                     ((search-and-modify heap size new-key old-key value 0)
                      (setf (gethash heap-id *heaps*)
                            (list 'heap heap-id size heap))
                      T))))))

;search-and-modify++
(defun search-and-modify (heap size new-key old-key value index)
                 (cond
                  ((< index size)
                   (cond
                    ;scorro tutto l'array cercando un elemento
                    ;che sia uguale ad old-key.
                    ;Non solo, controllo anche che il
                    ;valore di new-key sia minore di quello di
                    ;old-key in tal caso lo modifico (lo sostituisco
                    ;con un valore più piccolo).
                    ((and (eql (first (aref heap index)) old-key)
                          (eql (second (aref heap index)) value)
                          (< (fifth new-key) (fifth old-key)))
                     (setf (aref heap index) (list new-key value))
                     ;richiamo l'heapify per mantenere la
                     ;struttura dati ordinata.
                     (heapify heap size index)
                     T)
                    (T 
                     (search-and-modify heap size new-key old-key value
                                        (+ index 1)))))))

;heap-print
(defun heap-print (heap-id)
  (cond
   ((gethash heap-id *heaps*)
    (let
        ((heap (fourth (gethash heap-id *heaps*)))
         (size (third (gethash heap-id *heaps*))))
      (print-array heap size 0)
      T))))

;print-array++
(defun print-array (heap size index)
  (cond
   ((< index size)
    (format t "~S: chiave ~S, valore ~S~%" index (first (aref heap index)) 
             (second (aref heap index)))
    (print-array heap size (+ index 1)))))

;heapify++
(defun heapify (heap size index)
  (cond
   ((> index 0)
    (let* 
        ;recupero l'informazione sul nodo corrente visitato,
        ;sul suo nodo figlio a sinistra e sul suo nodo figlio a destra
        ((left (aref heap (- (* index 2) 1)))
           (right (aref heap (* index 2)))
           (current (aref heap (- index 1))))
         (cond
          ;controllo che il nodo destro faccia parte dello heap
          ((< (* index 2) size)
           (cond 
            ;confronto i valori dei tre nodi.
            ;primo caso il nodo a sinistra è il minore
            ((and (< (fifth (first left)) (fifth (first current)))
                  (<= (fifth (first left)) (fifth (first right))))
             ;sostituisco i valori
             (setf (aref heap (- index 1)) left) 
              (setf (aref heap (- (* index 2) 1)) current)
              ;riordino lo heap
              (heapify heap size (floor index 2)))
            ;secondo caso il nodo a destra è il minore
            ((and (< (fifth (first right)) (fifth (first current)))
                  (<= (fifth (first right)) (fifth (first left))))
             ;ripeto la prodcedura
              (setf (aref heap (- index 1)) right)
              (setf (aref heap (* index 2)) current)
              (heapify heap size (floor index 2)))))
          ;qualora solo il nodo sinistro del nodo current
          ;facesse parte dello heap
          ((< (- (* index 2) 1) size)
           (cond
            ;confronto se il nodo figlio a sinistra e minore
            ;del nodo padre (current)
            ;anche qui nel caso sostituisco i valori e richiamo l'heapify
            ((< (fifth (first left)) (fifth (first current))) 
               (setf (aref heap (- index 1)) left)
               (setf (aref heap (- (* index 2) 1)) current)
               (heapify heap size (floor index 2))))))))))


;topdown-heapify++
(defun topdown-heapify (heap size index)
  (cond
   ((<= index size) 
    (cond
     ;caso in cui un nodo ha sia un figlio sinistro che destro
     ((< (* index 2) size)
      (let 
          ((left (aref heap (- (* index 2) 1)))
           (right (aref heap (* index 2)))
           (current (aref heap (- index 1))))
        (cond
         ;eseguiamo anche qui i controlli per capire chi è il minore,
         ;se necessario aggiorniamo lo heap e richiamiamo ricorsivamente 
         ;la funzione
         ((and 
           (< (fifth (first left)) (fifth (first current)))
           (<= (fifth (first left)) (fifth (first right))))
          (setf (aref heap (- index 1)) left)
          (setf (aref heap (- (* index 2) 1)) current)
          (topdown-heapify heap size (* index 2)))
         ((and
           (< (fifth (first right)) (fifth (first left)))
           (<= (fifth (first right)) (fifth (first current))))
          (setf (aref heap (- index 1)) right)
          (setf (aref heap (* index 2)) current)
          (topdown-heapify heap size (* index 2))))))
     ;caso in cui un nodo ha solo un figlio sinistro
     ((< (- (* index 2) 1) size)
      (let
          ((left (aref heap (- (* index 2) 1)))
           (current (aref heap index)))
        (cond
         ((< (fifth (first left)) (fifth (first current)))
          (setf (aref heap (- index 1)) left)
          (setf (aref heap (- (* index 2) 1)) current)
          (topdown-heapify heap size (+ (* index 2) 1))))))))))


;-----------------------prim-----------------

;mst-vertex-key 
(defun mst-vertex-key (graph-id vertex-id)
  (cond
   ;controlliamo che il vertice non sia già stato visitato
   ;nel caso lo aggiungiamo alla hash-table *verte-keys*
   ((null (gethash (list graph-id vertex-id) *visited*))
    (let*
        ((heap (gethash graph-id *heaps*))
         (value (heap-head graph-id))
         (key (first value)))
      (setf (gethash (list graph-id vertex-id) *vertex-keys*)
            (list (fifth key) vertex-id))
      (fifth key)))
  ;altrimenti restituiamo il risultato già esistente nella hash-table
  (T (first (gethash (list graph-id vertex-id) *vertex-keys*)))))

;mst-previous
(defun mst-previous (graph-id vertex-id)
  (cond
   ;controlliamo che il vertice non sia già stato visitato.
   ;Qualora fosse un nuovo nodo lo aggiungiamo nella hash-table
   ;*previous* con il suo previous
   ((null (gethash (list graph-id vertex-id) *visited*))
    (let*
        ((heap (gethash graph-id *heaps*))
         (value (heap-head graph-id)))
      (cond
       ((eql vertex-id (fourth (first value)))
        (setf (gethash (list graph-id vertex-id) *previous*)
              (list graph-id vertex-id (third (first value))))))
      (setf (gethash (list graph-id vertex-id) *visited*)
            (list graph-id vertex-id))
      (third (first value))))
   ;altrimenti ritorniamo il valore già presente nella hash-table
   (T (third (gethash (list graph-id vertex-id) *previous*)))))



;mst-prim 
(defun mst-prim (graph-id source)
  (cond
   ;controllo se siamo all'inizio dell'algoritmo
   ((null (gethash (list graph-id 'prim_start) *visited*))
    (cond
     ;se il grafo non ha vertici non ha senso far partire l'algoritmo
     ((null (graph-vertices graph-id)) nil)
     ;altrimenti parte una prima fase dove prepariamo 
     ;l'inizio dell'esecuzione
     (T 
      (new-heap graph-id (length (graph-vertices graph-id)))
       ;utilizzo una entry speciale nella hash-table per 
       ;mantenere traccia dell'inizio dell'esecuzione dell'algoritmo
      (setf (gethash (list graph-id 'prim_start) *visited*)
            (list graph-id 'prim_start))
      (heap-insert graph-id (list 'arc graph-id source source 0) source)         
      ;il primo nodo in assoluto avrà anch'esso una entry speciale
      ;sia nella hash-table *vertex-keys* dove sarà associato
      ;la key 0.
      (setf (gethash (list graph-id source) *vertex-keys*)
            (list 0 source))
      ;sia nella hash-table *previous* dove avrà se stesso
      ;come previous
      (setf (gethash (list graph-id source) *previous*)
            (list graph-id source source))
      (mst-prim graph-id source))))
   
   ;se non siamo all'inizio
   (T
    (cond
     ;controllo anzitutto che lo heap non sia vuoto, altrimenti
     ;vorrebbe dire che l'algoritmo deve terminare
     ((heap-empty graph-id) 
      (heap-delete graph-id)
      nil)
     (T
      (cond 
       ;controllo che il nodo che sto esaminando
       ;non sia già stato visitato
       ((null (gethash (list graph-id source) *visited*))
       (let* 
            ;recupero la lista di vertici adiacenti
           ((adjacent (graph-vertex-adjacent graph-id source))
            ;trovo eventuali archi che mi portano a source 
            ;nella forma (arc graph-id nodo-adiacente source peso)
            (adjacent-arcs
             (find-alternative-routes source adjacent graph-id))
            ;trovo gli archi che portano ai nodi adiacenti 
            ;nella forma (arc graph-id source nodo-adiacente peso)
            (neighbors (graph-vertex-neighbors graph-id source))
            ;unisco tutti gli archi che ho trovato
            (available-arcs (append neighbors adjacent-arcs))
            ;li aggiungo tutti nello heap
            (a (heap-multiple-insert graph-id available-arcs))
            ;recupero l'arco in testa allo heap
            (head (heap-head graph-id)))
         ;lo aggiungo a *vertex-keys*
         (mst-vertex-key graph-id (second head))
         ;lo aggiungo a *previous*
         (mst-previous graph-id (second head))
         ;mantengo traccia del fatto
         ;che è stato visitato aggiungendolo 
         ;a *visited*
         (setf (gethash (list graph-id source) *visited*)
               (list graph-id source))
         ;lo estraggo rimuovendolo dallo heap, dato che ora 
         ;è stato visitato e quindi non ci serve più
         (heap-extract graph-id)
         ;richiamo l'agoritmo
         (mst-prim graph-id  (second (heap-head graph-id)))))
       ;se sto visitando un nodo già visitato in precedenza
       ;allora proseguiamo eliminandolo dallo heap
       (T (heap-extract 'p)
          (mst-prim graph-id (heap-head 'p)))))))))

;mst-get
(defun mst-get (graph-id)
  (let
      ;recuperiamo tutte le chiavi e tutti i previous
      ((keys ())
       (previous ()))
    (maphash (lambda (key value)
               (cond 
                ((eql (first key) graph-id)
                 (push value keys)))) *vertex-keys*)
    (maphash (lambda (key value)
               (cond 
                ((eql (first key) graph-id)
                 (push value previous)))) *previous*)
    ;sfruttiamo il metodo get-arcs per ricostruire gli archi 
    ;associati alle varie chiavi e ai vari preivous,
    ;dopodichè li stampiamo eseguendo prima delle operazioni di sorting 
     (cdr (sort 
           (sort (get-arcs graph-id keys previous) 
             (lambda (arc1 arc2) 
               (cond
                ((and (numberp (fourth arc1))
                      (numberp (fourth arc2))
                             (< (fourth arc1) (fourth arc2)))
                    T)
                ((and (not (numberp (fourth arc1)))
                      (not (numberp (fourth arc2)))
                           (string< (fourth arc1) (fourth arc2)))
                 T))))
      (lambda (arc1 arc2) (< (fifth arc1) (fifth arc2)))))))

;get-arcs++
(defun get-arcs (graph-id keys previous)
  (cond
   ((null keys) nil)
   ;applichiamo ad ogni elemento di keys la funzione
   ;build-arc-info così da ricostruire l'arco associato
   (T (append (build-arc-info graph-id (car keys) previous) 
              (get-arcs graph-id (cdr keys) previous)))))

;build-arc-info++
(defun build-arc-info (graph-id key previous)
   (cond
    ((null previous) nil)
    (T 
     ;scorriamo tutta la lista cercando quei previous 
     ;che hanno lo stesso vertice (secondo parametro)
     ;di quello di keys (secondo parametro).
     (cond
      ((eql (second key) (second (car previous)))
       (append 
        ;l'informazione contenuta in key e 
        ;nel car di previous è poi utilizzata
        ;per ricostruire l'arco associato
        (list (list 'arc graph-id (third (car previous)) 
                    (second (car previous)) (first key)))
               (build-arc-info graph-id key (cdr previous))))
      (T (build-arc-info graph-id key (cdr previous)))))))

;end of file - mst.lisp





















