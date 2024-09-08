#lang racket

; Definir el carácter de espacio vacío, que se usará para representar las celdas vacías del tablero
(define VACIO " ")

; Función para crear un tablero vacío de tamaño m x n, usando listas anidadas
; m: número de filas, n: número de columnas
(define (crear-tablero m n)
  (if (= m 0)
      '()  ; Caso base: si no quedan filas por crear, devolver una lista vacía
      (cons (crear-fila n) (crear-tablero (- m 1) n))))  ; Crear una fila y seguir con la siguiente

; Función para crear una fila de tamaño n llena de valores "VACIO"
(define (crear-fila n)
  (if (= n 0)
      '()  ; Caso base: si no quedan más columnas, devolver lista vacía
      (cons VACIO (crear-fila (- n 1)))))  ; Añadir un espacio vacío a la fila y continuar

; Función para obtener el valor de una posición específica en el tablero
; fila y columna son índices de las posiciones en el tablero
(define (obtener-valor tablero fila columna)
  (list-ref (list-ref tablero fila) columna))  ; Obtener la fila y luego la columna en esa fila

; Función para establecer un valor en una posición específica del tablero
; tablero: la lista de listas que representa el tablero
; fila, columna: la posición donde queremos colocar un valor
; valor: el valor que queremos colocar
(define (establecer-valor tablero fila columna valor)
  (define (actualizar-fila f)
    (if (= f fila)
        (actualizar-columna (list-ref tablero f) columna valor 0)  ; Actualizar la fila que corresponde
        (list-ref tablero f)))  ; Devolver la fila sin cambios si no es la que estamos actualizando
  ; Función auxiliar que actualiza una columna específica de una fila
  (define (actualizar-columna fila-actual col val idx)
    (if (null? fila-actual)
        '()  ; Caso base: si la fila está vacía, devolver una lista vacía
        (if (and (number? col) (number? idx) (= idx col))  ; Verificación de índices
            (cons val (cdr fila-actual))  ; Reemplazar el valor en la columna deseada
            (cons (car fila-actual) (actualizar-columna (cdr fila-actual) col val (+ idx 1))))))  ; Seguir recorriendo la fila
  (actualizar-tablero tablero actualizar-fila 0))  ; Actualizar el tablero aplicando la nueva fila

; Función que actualiza el tablero, aplicando una función de actualización fila por fila
(define (actualizar-tablero tablero func idx)
  (if (null? tablero)
      '()  ; Caso base: si el tablero está vacío, devolver una lista vacía
      (cons (func idx) (actualizar-tablero (cdr tablero) func (+ idx 1)))))  ; Aplicar la función a la primera fila y seguir con el resto

; Función que verifica si una posición del tablero está vacía
(define (esta-vacio? tablero fila columna)
  (equal? (obtener-valor tablero fila columna) VACIO))  ; Devuelve #t si el valor es "VACIO"

; Función que verifica si el tablero está lleno (sin posiciones VACÍAS)
(define (tablero-lleno? tablero)
  ; Función auxiliar que verifica si una fila está llena (sin valores VACIOS)
  (define (fila-llena? fila)
    (if (null? fila)
        #t  ; Caso base: una fila vacía está "llena" porque no tiene vacíos
        (and (not (equal? (car fila) VACIO)) (fila-llena? (cdr fila)))))  ; Verifica cada valor en la fila
  ; Función auxiliar que verifica si todas las filas están llenas
  (define (tablero-lleno-aux filas)
    (if (null? filas)
        #t  ; Caso base: si no hay filas restantes, el tablero está lleno
        (and (fila-llena? (car filas)) (tablero-lleno-aux (cdr filas)))))  ; Verifica fila por fila
  (tablero-lleno-aux tablero))  ; Inicia la verificación para todo el tablero

; Función que verifica si hay una victoria en una fila dada
; fila: índice de la fila a verificar
; jugador: "X" o "O" (el jugador actual)
(define (verificar-fila tablero fila jugador)
  ; Función auxiliar que recorre las celdas de la fila, buscando tres en línea del mismo jugador
  (define (verificar-celdas columna cuenta)
    (cond
      ((= cuenta 3) #t)  ; Si el contador llega a 3, hay victoria
      ((>= columna (length (car tablero))) #f)  ; Si se llega al final de la fila, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)
       (verificar-celdas (+ columna 1) (+ cuenta 1)))  ; Si se encuentra una celda del jugador, se incrementa el contador
      (else (verificar-celdas (+ columna 1) 0))))  ; Si no coincide, se reinicia el contador
  (verificar-celdas 0 0))

; Función que verifica si hay una victoria en una columna dada
(define (verificar-columna tablero columna jugador)
  ; Función auxiliar que recorre las filas en la columna dada
  (define (verificar-celdas fila cuenta)
    (cond
      ((= cuenta 3) #t)  ; Tres en línea, hay victoria
      ((>= fila (length tablero)) #f)  ; Si llegamos al final de las filas, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)
       (verificar-celdas (+ fila 1) (+ cuenta 1)))  ; Si hay una coincidencia, se incrementa el contador
      (else (verificar-celdas (+ fila 1) 0))))  ; Si no coincide, se reinicia el contador
  (verificar-celdas 0 0))

; Función que verifica si hay una victoria en las diagonales
; Se verifican dos diagonales: la principal (de arriba izquierda a abajo derecha)
; y la secundaria (de arriba derecha a abajo izquierda)
(define (verificar-diagonales tablero jugador)
  ; Verificar la diagonal principal
  (define (verificar-diagonal-principal fila columna cuenta)
    (cond
      ((= cuenta 3) #t)  ; Si hay tres en línea, hay victoria
      ((or (>= fila (length tablero)) (>= columna (length (car tablero)))) #f)  ; Si salimos del tablero, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)
       (verificar-diagonal-principal (+ fila 1) (+ columna 1) (+ cuenta 1)))  ; Continuar verificando la diagonal
      (else (verificar-diagonal-principal (+ fila 1) (+ columna 1) 0))))
  
  ; Verificar la diagonal secundaria
  (define (verificar-diagonal-secundaria fila columna cuenta)
    (cond
      ((= cuenta 3) #t)  ; Si hay tres en línea, hay victoria
      ((or (>= fila (length tablero)) (< columna 0)) #f)  ; Si salimos del tablero, no hay victoria
      ((equal? (obtener-valor tablero fila columna) jugador)
       (verificar-diagonal-secundaria (+ fila 1) (- columna 1) (+ cuenta 1)))  ; Continuar verificando la diagonal secundaria
      (else (verificar-diagonal-secundaria (+ fila 1) (- columna 1) 0))))
  
  ; Hay victoria si cualquiera de las dos diagonales tiene tres en línea
  (or (verificar-diagonal-principal 0 0 0)
      (verificar-diagonal-secundaria 0 (- (length (car tablero)) 1) 0)))

; Función que verifica si un jugador ha ganado
; jugador: "X" o "O"
(define (verificar-victoria tablero jugador)
  ; Verificar todas las filas
  (define (verificar-filas idx)
    (if (>= idx (length tablero))
        #f  ; Si se han verificado todas las filas sin victoria, devolver #f
        (or (verificar-fila tablero idx jugador)
            (verificar-filas (+ idx 1)))))  ; Verificar la siguiente fila
  
  ; Verificar todas las columnas
  (define (verificar-columnas idx)
    (if (>= idx (length (car tablero)))
        #f  ; Si se han verificado todas las columnas sin victoria, devolver #f
        (or (verificar-columna tablero idx jugador)
            (verificar-columnas (+ idx 1)))))  ; Verificar la siguiente columna
  
  ; Verificar filas, columnas y diagonales
  (or (verificar-filas 0)
      (verificar-columnas 0)
      (verificar-diagonales tablero jugador)))

; Evaluar una posición, asignando puntajes según las posibilidades de victoria
; El jugador que tiene una victoria obtiene +10, el oponente obtiene +5
(define (evaluar-posicion tablero fila columna jugador)
  (define oponente (if (equal? jugador "X") "O" "X"))  ; Definir el oponente
  (+ (if (verificar-victoria (establecer-valor tablero fila columna jugador) jugador) 10 0)  ; Puntaje para el jugador
     (if (verificar-victoria (establecer-valor tablero fila columna oponente) oponente) 5 0)))  ; Puntaje para el oponente

; Función que encuentra el mejor movimiento para la IA evaluando todas las posiciones vacías
(define (encontrar-mejor-movimiento tablero)
  ; Función auxiliar que encuentra la posición con la mayor evaluación
  (define (encontrar-max-eval fila columna max-eval mejor-movimiento)
    (cond
      ((>= fila (length tablero)) mejor-movimiento)  ; Si se revisaron todas las filas, devolver el mejor movimiento
      ((>= columna (length (car tablero))) (encontrar-max-eval (+ fila 1) 0 max-eval mejor-movimiento))  ; Si se revisaron todas las columnas, pasar a la siguiente fila
      ((not (esta-vacio? tablero fila columna)) (encontrar-max-eval fila (+ columna 1) max-eval mejor-movimiento))  ; Si la celda no está vacía, continuar con la siguiente
      (else
       (define eval (evaluar-posicion tablero fila columna "O"))  ; Evaluar la posición actual
       (if (> eval max-eval)
           (encontrar-max-eval fila (+ columna 1) eval (cons fila columna))  ; Si es mejor, actualizar mejor movimiento
           (encontrar-max-eval fila (+ columna 1) max-eval mejor-movimiento)))))  ; Continuar buscando
  (encontrar-max-eval 0 0 -1 #f))  ; Iniciar la búsqueda desde la fila y columna 0

; Bucle principal del juego, alternando turnos entre el jugador y la IA
(define (jugar-partida m n)
  (define tablero (crear-tablero m n))  ; Crear el tablero inicial
  
  ; Bucle del juego que maneja turnos y verifica el estado del tablero
  (define (bucle-juego tablero-actual jugador-actual)
    (mostrar-tablero tablero-actual)  ; Mostrar el estado actual del tablero
    (cond
      ((verificar-victoria tablero-actual (if (equal? jugador-actual "X") "O" "X"))
       (printf "¡~a gana!\n" (if (equal? jugador-actual "X") "O" "X")))  ; Si hay victoria, anunciar el ganador
      ((tablero-lleno? tablero-actual) (printf "¡Es un empate!\n"))  ; Si el tablero está lleno, anunciar empate
      (else
       (if (equal? jugador-actual "X")
           (comenzar-turno-jugador tablero-actual)  ; Turno del jugador
           (comenzar-turno-ia tablero-actual)))))  ; Turno de la IA
  
  ; Manejar el turno del jugador
  (define (comenzar-turno-jugador tablero-actual)
    (define movimiento (obtener-movimiento-jugador tablero-actual))  ; Obtener movimiento del jugador
    (bucle-juego (establecer-valor tablero-actual (car movimiento) (cadr movimiento) "X") "O"))  ; Actualizar tablero y cambiar turno
  
  ; Manejar el turno de la IA
  (define (comenzar-turno-ia tablero-actual)
    (define movimiento-ia (encontrar-mejor-movimiento tablero-actual))  ; Encontrar el mejor movimiento para la IA
    (printf "La IA mueve a: ~a,~a\n" (+ (car movimiento-ia) 1) (+ (cdr movimiento-ia) 1))  ; Mostrar la jugada de la IA
    (bucle-juego (establecer-valor tablero-actual (car movimiento-ia) (cdr movimiento-ia) "O") "X"))  ; Actualizar tablero y cambiar turno
  
  (bucle-juego tablero "X"))  ; Iniciar el bucle con el jugador "X"

; Función para mostrar el tablero en la consola
(define (mostrar-tablero tablero)
  ; Mostrar cada fila del tablero
  (define (mostrar-fila fila)
    (if (null? fila)
        (newline)  ; Si la fila está vacía, pasar a la siguiente línea
        (begin
          (display (if (equal? (car fila) VACIO) "." (car fila)))  ; Mostrar "." en lugar de celdas vacías
          (display " ")  ; Añadir espacio entre celdas
          (mostrar-fila (cdr fila)))))  ; Mostrar el siguiente valor de la fila
  ; Mostrar todas las filas del tablero
  (define (mostrar-filas filas)
    (if (null? filas)
        (void)  ; Si no hay más filas, no hacer nada
        (begin
          (mostrar-fila (car filas))  ; Mostrar la fila actual
          (mostrar-filas (cdr filas)))))  ; Mostrar el resto de las filas
  (mostrar-filas tablero))  ; Iniciar la visualización del tablero

; Función auxiliar para obtener el movimiento del jugador
(define (obtener-movimiento-jugador tablero)
  ; Solicitar al jugador ingresar su movimiento y validarlo
  (define (obtener-movimiento-valido)
    (printf "Ingresa tu movimiento (fila columna): ")
    (define entrada (read-line))  ; Leer la entrada del usuario
    (define movimiento (convertir-entrada-a-movimiento entrada))  ; Convertir la entrada en una lista de coordenadas
    (if (and (lista-de-dos? movimiento)
             (movimiento-valido? tablero movimiento))  ; Verificar si el movimiento es válido
        movimiento
        (begin
          (printf "Movimiento inválido. Intenta de nuevo.\n")  ; Si no es válido, solicitar de nuevo
          (obtener-movimiento-valido))))
  (obtener-movimiento-valido))

; Función que convierte la entrada del jugador en una lista de coordenadas (sin usar map)
(define (convertir-entrada-a-movimiento entrada)
  (define (procesar-caracteres idx numeros)
    (if (>= idx (string-length entrada))
        numeros  ; Caso base: devolver los números procesados
        (if (char-numeric? (string-ref entrada idx))
            (procesar-caracteres (+ idx 1) 
                                 (cons (- (string->number (substring entrada idx (+ idx 1))) 1)
                                       numeros))  ; Convertir cada dígito en número y restar 1 para los índices
            (procesar-caracteres (+ idx 1) numeros))))  ; Seguir procesando el resto de la cadena
  (reverse (procesar-caracteres 0 '())))

; Función para verificar si la lista tiene exactamente dos elementos (representando fila y columna)
(define (lista-de-dos? lst)
  (and (list? lst) (= (length lst) 2)))

; Verificar si el movimiento es válido (dentro del rango del tablero y la posición está vacía)
(define (movimiento-valido? tablero movimiento)
  (and (>= (car movimiento) 0) (< (car movimiento) (length tablero))
       (>= (cadr movimiento) 0) (< (cadr movimiento) (length (car tablero)))
       (esta-vacio? tablero (car movimiento) (cadr movimiento))))

; Iniciar el juego de Tic-Tac-Toe
(define (TTT m n)
  (if (and (>= m 3) (<= m 10) (>= n 3) (<= n 10))  ; Verificar que el tamaño del tablero esté en el rango permitido
      (jugar-partida m n)  ; Iniciar el juego
      (printf "Tamaño de tablero inválido. Por favor, usa dimensiones entre 3x3 y 10x10.\n")))  ; Mensaje de error para tamaños no válidos
