; modelar matriz

;(defvar mat   '((#\0 #\0 #\0   0   0 #\0)
;                (#\0   2   0   4   0 #\0)
;                (  0   3   0   6   0   4)
;                (  0   0   0   0   0   0)
;                (#\1   0   2   0   0 #\0)
;                (#\0   0   0 #\0 #\0 #\5)
;                )
;)
;(defvar mat    '((#\0   0   0 #\0 #\7   0   0 #\0 #\0)
;                 (  0   0   0   0   0   0   0   0   0)
;                 (  0   4 #\0   0   0 #\0 #\0   0   8)
;                 (#\0   0   0   0 #\0   0   0   0   0)
;                 (#\0 #\8   2   0   6   0   0 #\1 #\0)
;                 (  1   0   0   0 #\0   0   7   0 #\0)
;                 (  0   0 #\9 #\0   0   0 #\0   0   6)
;                 (  0   0   0   0   0   0   0   0   0)
;                 (#\0 #\0   0   9 #\0 #\0   0   0 #\4)
;                 )
;)
;(defvar mat   '((  3   4 #\0   0   0 #\0)
;                (  0   0 #\1   6   0 #\0)
;                (  0   0   0   0   0 #\0)
;                (#\0   0   0   0   1   0)
;                (#\0   0   3 #\0   0   0)
;                (#\0   0   0 #\0   0   0)
;                )
;)
(defvar mat   '((#\0   0   0 #\1 #\0 #\0)
                (#\0   0   0   0   5   0)
                (#\0   0   1   0   0   0)
                (  4   0   0   0   0 #\0)
                (  0   6   5   0   0 #\0)
                (#\0 #\0 #\0   0   1 #\4)
                )
)
;(defvar mat    '((#\1   0   0   0   0   0   0   7   0)
 ;                (  0   1 #\4 #\0   0   0   5 #\0 #\0)
  ;               (  0 #\5 #\0   0   0   0 #\0 #\0   0)
   ;              (  0 #\0   0   8   5   0 #\0   0   3)
    ;             (#\0   0   0   0 #\0   0   0   0 #\0)
     ;            (  0   0 #\6   4   0   0   0 #\0   0)
      ;           (  0 #\0 #\0   0   3   0 #\0 #\0   5)
       ;          (#\0 #\7   0   0   4 #\0 #\0   0   0)
        ;         (  0   4   0   0   0   1   8   6 #\9)
         ;        )
;)

; seta size
(defvar size (length mat))

; atualiza matriz
(defun atualiza_mat(i j n)
    (setf (nth i mat) (atualiza_linha (nth i mat) j n 0))
)
; sub-metodo de atualizar matriz
(defun atualiza_linha(vet j n cont)
    (cond 
        ((null vet) ()) 
        ((= j cont) (cons n (atualiza_linha (cdr vet) j n (+ 1 cont))))
        (t (cons (car vet) (atualiza_linha (cdr vet) j n (+ 1 cont))))
    )
)

; coluna, nao tao imediato como linha
(defun get_col(i y)
    (cond
        ((>= i size) ())
        (t (cons (nth y (nth i mat)) (get_col (+ 1 i) y)))
    )
)
; menor da lista /= 0
(defun menor(lista menor)
    (if (null lista)
        menor
        (if (and (> menor (car lista)) (/= 0 (car lista)))
            (menor (cdr lista) (car lista))
            (menor (cdr lista) menor)
        )
    )
)

; checa se n esta no range valido
(defun testa_ta_no_range(vet n)
    (if (and (>= n (nth 0 vet)) (<= n (nth 1 vet)))
        T
        NIL
    )
) 
; retorna vetor adjacente relativo a uma linha/coluna e um x que é a posição, é um vetor dos digitos ao redor de x
(defun vetor_adjacente(vet cont x b out)
    (cond
        ((>= cont size) out)
        ((and (typep (nth cont vet) 'character) b) out) ; casos finais
        ((and (typep (nth cont vet) 'integer) (= cont x)) (vetor_adjacente vet (+ 1 cont) x T (cons (nth cont vet) out)))
        ((and (typep (nth cont vet) 'integer) (/= cont x)) (vetor_adjacente vet (+ 1 cont) x b (cons (nth cont vet) out)))
        (t (vetor_adjacente vet (+ 1 cont) x b ()))
    )
)
; corrige valores  > size ou < 1
(defun corrige_valores(x)
    (if (> x size)
        size
        (if (< x 1)
            1
            x
        )
    )
)
; defino variaveis
(defvar max_el size)
(defvar min_el 1)
; acho intervalos relativos a um vetor de adjacencia
(defun acha_intervalos(vet)
    (if (null vet)
        (cons max_el (list min_el))
        (let ((m (apply 'max vet)))
            (if (/= m 0)
                (cons (corrige_valores (+ (- m (length vet)) 1)) (list (corrige_valores (- (+ (menor vet (+ 1 size)) (length vet)) 1))))
                (cons min_el (list max_el))
            )
        )
    )
)
; testo se n é repetido na linha/coluna
(defun testa_repetido(vet n)
    (if (null vet)
        NIL
        (if (typep (car vet) 'character)
            (if (char= (car vet) (digit-char n))
                T
                (testa_repetido (cdr vet) n)
            )
            (if (= (car vet) n)
                T
                (testa_repetido (cdr vet) n)
            )
        )
    )
)
; funcao de checagem
(defun possivel(i j n)
    (cond
        ((or (testa_repetido (nth i mat) n) (testa_repetido (get_col 0 j) n)) NIL)
        ((and (testa_ta_no_range (acha_intervalos (vetor_adjacente (nth i mat) 0 j NIL ())) n) (testa_ta_no_range (acha_intervalos (vetor_adjacente (get_col 0 j) 0 i NIL ())) n)) T)
        (t NIL)
    )
)


; funcao de backtracking
(defun resolve ()
    (progn 
        (do ((i 0 (+ 1 i)))
            ((> i (- size 1)))
            (do ((j 0 (+ 1 j)))
                ((> j (- size 1)))
                (let ((z (nth j (nth i mat))))
                    (if (typep z 'integer)
                        (if (= z 0)
                            (progn
                                (do ((n 1 (+ 1 n)))
                                    ((> n size))
                                    (if (possivel i j n)
                                        (progn
                                            (atualiza_mat i j n)
                                            (resolve)
                                            (atualiza_mat i j 0)
                                        )
                                        (go end3)
                                    )
                                end3)
                                (return-from resolve)
                            )
                            (go end2)
                        )
                        (go end2)
                    )
                )
            end2)
        end1)
        (do ((p 0 (+ 1 p)))
            ((> p (- size 1)))
            (write-line (write-to-string (nth p mat)))
        )
    )
)
; main
(defun main()
    (time (resolve))
)

(main)


; METODOS (ERRADOS) OTIMIZACAO

#||
; metodo para checar se um elemento em i j é definitivamente o certo, é chamado pelo "loop_compartimento()", que por sua vez
; seria chamado no inicio de "resolve()" para diminuir possibilidades, e "loop_compartimento()" chamaria solve caso desse certo,
; de forma a checar desde o inicio a matriz para achar novos valores certos, caso desse errado, caso não exista digitos certos
; realiza uma iteração por força bruta e tenta novamente chamar "loop_compartimento()", caso não seja possivel realizar força bruta
; retorna e realiza back-tracking. 
(defun compartimento(i j)
    (progn
        (let* ((lista_completa_row ())
             (lista_completa_col ())
             (lista_adjacente_row (vetor_adjacente (nth i mat) 0 j NIL ()))
             (lista_adjacente_col (vetor_adjacente (get_col 0 j) 0 i NIL ()))
             (intervalo_row (acha_intervalos lista_adjacente_row))
             (intervalo_col (acha_intervalos lista_adjacente_col)))
            (progn
                (do ((i (nth 0 intervalo_row) (+ 1 i)))
                    ((> i (nth 1 intervalo_row)))
                    (setq lista_completa_row (cons i lista_completa_row))
                    ;(write-line (write-to-string i))
                    ;(write-line (write-to-string lista_completa_row))
                )
                (do ((i (nth 0 intervalo_col) (+ 1 i)))
                    ((> i (nth 1 intervalo_col)))
                    (setq lista_completa_col (cons i lista_completa_col))
                )
                (let* ((possib_row (set-difference lista_completa_row lista_adjacente_row))
                     (possib_col (set-difference lista_completa_col lista_adjacente_col))
                     (intersec (intersection possib_row possib_col)))
                    (if (= 1  (length intersec))
                        (nth 0 intersec)
                        (- 1)
                    )
                )
            )
        )
    )
)   

; seria chamado no inicio de "resolve()"
(defun loop_compartimento()
    (do ((i 0 (+ 1 i)))
        ((> i (- size 1)))
        (do ((j 0 (+ 1 j)))
            ((> j (- size 1)))
            (let ((z (nth j (nth i mat))))
                (if (typep z 'integer)
                    (if (= z 0)
                        (let ((n (nth j (nth i mat))))
                            (if (/= n 0)
                                (progn
                                    (atualiza_mat i j n)
                                    (resolve)
                                    (atualiza_mat i j 0)
                                )
                                (go end2)
                            )
                        )
                        (go end2)
                    )
                    (go end2)
                )
            )
        end2)
    end1)
)
||#