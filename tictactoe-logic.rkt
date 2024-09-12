#lang racket

(provide crear-tablero
         obtener-valor
         establecer-valor
         esta-vacio?
         tablero-lleno?
         verificar-victoria
         encontrar-mejor-movimiento
         jugar-partida
         mostrar-tablero
         obtener-movimiento-jugador
         TTT)

; Definir el carácter de espacio vacío
(define VACIO " ")

; Función para crear un tablero vacío de tamaño m x n
; Cada fila contiene 'n' posiciones, y se crean 'm' filas
(define (crear-tablero m n)
  (if (= m 0)
      '()  ; Caso base: si m es 0, no hay filas
      (cons (crear-fila n) (crear-tablero (- m 1) n))))  ; Llama recursivamente para construir el tablero

; Función para crear una fila de tamaño n llena de valores "VACIO"
(define (crear-fila n)
  (if (= n 0)
      '()  ; Caso base: si n es 0, no hay más posiciones
      (cons VACIO (crear-fila (- n 1)))))  ; Agrega VACIO a cada posición de la fila

; Función para obtener el valor en una posición específica del tablero
; Accede a la fila y luego a la columna en esa fila
(define (obtener-valor tablero fila columna)
  (list-ref (list-ref tablero fila) columna))

; Función para establecer un valor en una posición específica del tablero
; Actualiza la fila correcta y luego la columna dentro de esa fila
(define (establecer-valor tablero fila columna valor)
  (define (actualizar-fila f)
    (if (= f fila)
        (actualizar-columna (list-ref tablero f) columna valor 0)  ; Actualiza la fila en la posición 'fila'
        (list-ref tablero f)))  ; Mantiene las otras filas iguales
  (define (actualizar-columna fila-actual col val idx)
    (if (null? fila-actual)
        '()
        (if (and (number? col) (number? idx) (= idx col))  ; Verifica que se actualiza en la columna correcta
            (cons val (cdr fila-actual))  ; Actualiza la columna
            (cons (car fila-actual) (actualizar-columna (cdr fila-actual) col val (+ idx 1))))))  ; Continua con las otras columnas
  (actualizar-tablero tablero actualizar-fila 0))  ; Aplica la actualización al tablero

; Función que actualiza el tablero aplicando una función fila por fila
(define (actualizar-tablero tablero func idx)
  (if (null? tablero)
      '()
      (cons (func idx) (actualizar-tablero (cdr tablero) func (+ idx 1)))))

; Verifica si una posición en el tablero está vacía (igual a "VACIO")
(define (esta-vacio? tablero fila columna)
  (equal? (obtener-valor tablero fila columna) VACIO))

; Verifica si todo el tablero está lleno (sin espacios vacíos)
(define (tablero-lleno? tablero)
  (define (fila-llena? fila)
    (if (null? fila)
        #t
        (and (not (equal? (car fila) VACIO)) (fila-llena? (cdr fila)))))  ; Revisa cada celda de la fila
  (define (tablero-lleno-aux filas)
    (if (null? filas)
        #t
        (and (fila-llena? (car filas)) (tablero-lleno-aux (cdr filas)))))  ; Revisa cada fila
  (tablero-lleno-aux tablero))

; Verifica si hay una victoria en una fila para el jugador dado
(define (verificar-fila tablero fila jugador)
  (define (verificar-celdas columna cuenta)
    (cond
      ((= cuenta 3) #t)  ; Si hay 3 en línea, el jugador gana
      ((>= columna (length (car tablero))) #f)  ; Si se sale del tablero, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)  ; Verifica si el jugador tiene esa posición
       (verificar-celdas (+ columna 1) (+ cuenta 1)))  ; Continúa verificando la siguiente celda
      (else (verificar-celdas (+ columna 1) 0))))  ; Reinicia el contador si no es del jugador
  (verificar-celdas 0 0))

; Verifica si hay una victoria en una columna para el jugador dado
(define (verificar-columna tablero columna jugador)
  (define (verificar-celdas fila cuenta)
    (cond
      ((= cuenta 3) #t)  ; Si hay 3 en línea, el jugador gana
      ((>= fila (length tablero)) #f)  ; Si se sale del tablero, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)  ; Verifica si el jugador tiene esa posición
       (verificar-celdas (+ fila 1) (+ cuenta 1)))  ; Continúa verificando la siguiente fila
      (else (verificar-celdas (+ fila 1) 0))))  ; Reinicia el contador si no es del jugador
  (verificar-celdas 0 0))

; Verifica si hay una victoria en las diagonales
(define (verificar-diagonales tablero jugador)
  (define filas (length tablero))
  (define columnas (length (car tablero)))
  
  ; Verifica la diagonal de izquierda a derecha (diagonal principal)
  (define (verificar-diagonales-izq-der)
    (define (verificar-desde fila-inicio col-inicio)
      (define (verificar fila col cuenta)
        (cond
          ((= cuenta 3) #t)  ; Si hay 3 en línea, el jugador gana
          ((or (>= fila filas) (>= col columnas)) #f)  ; Si se sale del tablero, no hay victoria
          ((equal? (obtener-valor tablero fila col) jugador)  ; Verifica si el jugador tiene esa posición
           (verificar (+ fila 1) (+ col 1) (+ cuenta 1)))  ; Continúa en la diagonal
          (else (verificar (+ fila 1) (+ col 1) 0))))
      (verificar fila-inicio col-inicio 0))
    
    (define (iterar-inicio fila col)
      (cond
        ((>= fila (- filas 2)) #f)  ; Limita la iteración para evitar revisiones innecesarias
        ((>= col (- columnas 2)) (iterar-inicio (+ fila 1) 0))
        ((verificar-desde fila col) #t)
        (else (iterar-inicio fila (+ col 1)))))
    
    (iterar-inicio 0 0))  ; Comienza desde la esquina superior izquierda
  
  ; Verifica la diagonal de derecha a izquierda (diagonal secundaria)
  (define (verificar-diagonales-der-izq)
    (define (verificar-desde fila-inicio col-inicio)
      (define (verificar fila col cuenta)
        (cond
          ((= cuenta 3) #t)
          ((or (>= fila filas) (< col 0)) #f)
          ((equal? (obtener-valor tablero fila col) jugador)
           (verificar (+ fila 1) (- col 1) (+ cuenta 1)))
          (else (verificar (+ fila 1) (- col 1) 0))))
      (verificar fila-inicio col-inicio 0))
    
    (define (iterar-inicio fila col)
      (cond
        ((>= fila (- filas 2)) #f)
        ((< col 2) (iterar-inicio (+ fila 1) (- columnas 1)))
        ((verificar-desde fila col) #t)
        (else (iterar-inicio fila (- col 1)))))
    
    (iterar-inicio 0 (- columnas 1)))  ; Comienza desde la esquina superior derecha
  
  (or (verificar-diagonales-izq-der) (verificar-diagonales-der-izq)))

; Verifica si un jugador ha ganado en filas, columnas o diagonales
(define (verificar-victoria tablero jugador)
  (define (verificar-filas idx)
    (if (>= idx (length tablero))
        #f
        (or (verificar-fila tablero idx jugador)
            (verificar-filas (+ idx 1)))))
  
  (define (verificar-columnas idx)
    (if (>= idx (length (car tablero)))
        #f
        (or (verificar-columna tablero idx jugador)
            (verificar-columnas (+ idx 1)))))
  
  (or (verificar-filas 0)
      (verificar-columnas 0)
      (verificar-diagonales tablero jugador)))

; Evalúa una posición según las posibles victorias, otorgando puntajes
(define (evaluar-posicion tablero fila columna jugador)
  (define oponente (if (equal? jugador "X") "O" "X"))  ; Define el jugador oponente
  (+ (if (verificar-victoria (establecer-valor tablero fila columna jugador) jugador) 10 0)
     (if (verificar-victoria (establecer-valor tablero fila columna oponente) oponente) 5 0)))

; Encuentra el mejor movimiento para la IA revisando todas las posiciones
(define (encontrar-mejor-movimiento tablero)
  (define (encontrar-max-eval fila columna max-eval mejor-movimiento)
    (cond
      ((>= fila (length tablero)) mejor-movimiento)  ; Si se revisan todas las filas, devuelve el mejor movimiento
      ((>= columna (length (car tablero))) (encontrar-max-eval (+ fila 1) 0 max-eval mejor-movimiento))
      ((not (esta-vacio? tablero fila columna)) (encontrar-max-eval fila (+ columna 1) max-eval mejor-movimiento))
      (else
       (define eval (evaluar-posicion tablero fila columna "O"))
       (if (> eval max-eval)
           (encontrar-max-eval fila (+ columna 1) eval (cons fila columna))  ; Si encuentra una mejor evaluación, actualiza
           (encontrar-max-eval fila (+ columna 1) max-eval mejor-movimiento)))))
  (encontrar-max-eval 0 0 -1 #f))

; Muestra el tablero en la consola
(define (mostrar-tablero tablero)
  (define (mostrar-fila fila)
    (if (null? fila)
        (newline)
        (begin
          (display (if (equal? (car fila) VACIO) "." (car fila)))  ; Muestra '.' en lugar de espacios vacíos
          (display " ")
          (mostrar-fila (cdr fila)))))
  (define (mostrar-filas filas)
    (if (null? filas)
        (void)
        (begin
          (mostrar-fila (car filas))
          (mostrar-filas (cdr filas)))))
  (mostrar-filas tablero))

; Obtiene el movimiento del jugador desde la entrada de la consola
(define (obtener-movimiento-jugador tablero)
  (define (obtener-movimiento-valido)
    (printf "Ingresa tu movimiento (fila columna): ")
    (define entrada (read-line))  ; Lee la entrada del jugador
    (define movimiento (convertir-entrada-a-movimiento entrada))
    (if (and (lista-de-dos? movimiento)
             (movimiento-valido? tablero movimiento))
        movimiento
        (begin
          (printf "Movimiento inválido. Intenta de nuevo.\n")
          (obtener-movimiento-valido))))  ; Solicita nuevamente si el movimiento es inválido
  (obtener-movimiento-valido))

; Convierte la entrada del jugador en una lista de coordenadas
(define (convertir-entrada-a-movimiento entrada)
  (define (procesar-caracteres idx numeros)
    (if (>= idx (string-length entrada))
        numeros
        (if (char-numeric? (string-ref entrada idx))
            (procesar-caracteres (+ idx 1) 
                                 (cons (- (string->number (substring entrada idx (+ idx 1))) 1)
                                       numeros))
            (procesar-caracteres (+ idx 1) numeros))))
  (reverse (procesar-caracteres 0 '())))  ; Devuelve las coordenadas en orden correcto

; Verifica si una lista tiene exactamente dos elementos
(define (lista-de-dos? lst)
  (and (list? lst) (= (length lst) 2)))

; Verifica si el movimiento es válido (dentro del tablero y en una posición vacía)
(define (movimiento-valido? tablero movimiento)
  (and (>= (car movimiento) 0) (< (car movimiento) (length tablero))
       (>= (cadr movimiento) 0) (< (cadr movimiento) (length (car tablero)))
       (esta-vacio? tablero (car movimiento) (cadr movimiento))))

; Bucle principal del juego que alterna entre el jugador y la IA
(define (jugar-partida m n)
  (define tablero (crear-tablero m n))
  
  (define (bucle-juego tablero-actual jugador-actual)
    (mostrar-tablero tablero-actual)
    (cond
      ((verificar-victoria tablero-actual (if (equal? jugador-actual "X") "O" "X"))
       (printf "¡~a gana!\n" (if (equal? jugador-actual "X") "O" "X")))  ; Verifica si alguien ganó
      ((tablero-lleno? tablero-actual) (printf "¡Es un empate!\n"))  ; Verifica si el tablero está lleno
      (else
       (if (equal? jugador-actual "X")
           (comenzar-turno-jugador tablero-actual)  ; Turno del jugador humano
           (comenzar-turno-ia tablero-actual)))))  ; Turno de la IA
  
  ; Lógica para el turno del jugador humano
  (define (comenzar-turno-jugador tablero-actual)
    (define movimiento (obtener-movimiento-jugador tablero-actual))
    (bucle-juego (establecer-valor tablero-actual (car movimiento) (cadr movimiento) "X") "O"))
  
  ; Lógica para el turno de la IA
  (define (comenzar-turno-ia tablero-actual)
    (define movimiento-ia (encontrar-mejor-movimiento tablero-actual))
    (printf "La IA mueve a: ~a,~a\n" (+ (car movimiento-ia) 1) (+ (cdr movimiento-ia) 1))  ; Muestra el movimiento de la IA
    (bucle-juego (establecer-valor tablero-actual (car movimiento-ia) (cdr movimiento-ia) "O") "X"))
  
  (bucle-juego tablero "X"))

; Función principal para iniciar el juego de Tic-Tac-Toe
(define (TTT m n)
  (if (and (>= m 3) (<= m 10) (>= n 3) (<= n 10))
      (jugar-partida m n)
      (printf "Tamaño de tablero inválido. Por favor, usa dimensiones entre 3x3 y 10x10.\n")))
