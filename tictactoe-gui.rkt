#lang racket/gui

(require racket/class)

; Importar las funciones necesarias del archivo de lógica del juego
(require "tictactoe-logic.rkt")

; Definir la clase para la ventana principal del juego Tic-Tac-Toe
(define tictactoe-frame%
  (class frame%
    ; Inicializar la ventana con el número de filas y columnas especificadas
    (init-field rows cols)
    
    ; Crear una nueva ventana con el título "Tic-Tac-Toe" y dimensiones 300x300
    (super-new [label "Tic-Tac-Toe"] [width 300] [height 300])
    
    ; Crear un tablero vacío de acuerdo a las dimensiones dadas
    (define board (crear-tablero rows cols))
    
    ; Crear un vector de botones del tamaño adecuado para el tablero
    (define buttons (make-vector (* rows cols) #f))
    
    ; Crear el panel principal que contendrá la cuadrícula y el botón de reinicio
    (define panel (new vertical-panel% [parent this] [alignment '(center center)]))
    
    ; Crear un panel adicional para la cuadrícula de botones (usando paneles anidados)
    (define grid-panel (new vertical-panel% [parent panel] [alignment '(center center)]))
    
    ; Función para actualizar la interfaz gráfica del tablero
    ; Esta función actualiza los botones con el valor actual del tablero (X, O o vacío)
    (define (update-gui)
      (for ([btn (in-vector buttons)]
            [i (in-range (* rows cols))])
        (let ([row (quotient i cols)]  ; Determina la fila correspondiente al índice
              [col (remainder i cols)]) ; Determina la columna correspondiente al índice
          ; Actualiza la etiqueta del botón en función del valor del tablero
          (send btn set-label 
                (let ([value (obtener-valor board row col)])
                  (if (equal? value " ") "" value))))))  ; Deja en blanco si está vacío, de lo contrario muestra X u O
    
    ; Función para manejar el clic en un botón del tablero
    ; Actualiza el tablero y verifica el estado del juego después de cada clic
    (define (button-click i)
      (let ([row (quotient i cols)]  ; Calcula la fila correspondiente al botón clickeado
            [col (remainder i cols)]) ; Calcula la columna correspondiente al botón clickeado
        ; Verifica si la posición está vacía antes de realizar cualquier movimiento
        (when (esta-vacio? board row col)
          ; Establecer el valor "X" para el jugador en la posición clickeada
          (set! board (establecer-valor board row col "X"))
          (update-gui)  ; Actualizar la interfaz después del movimiento del jugador
          ; Verificar si el jugador ha ganado o si el tablero está lleno
          (cond
            [(verificar-victoria board "X")  ; Si el jugador gana
             (message-box "Fin del juego" "¡Has ganado!")]
            [(tablero-lleno? board)  ; Si el tablero está lleno, es un empate
             (message-box "Fin del juego" "¡Empate!")]
            [else  ; Si el juego continúa, la IA realiza su movimiento
             (let ([ia-move (encontrar-mejor-movimiento board)])  ; Encontrar el mejor movimiento de la IA
               (set! board (establecer-valor board (car ia-move) (cdr ia-move) "O"))
               (update-gui)  ; Actualizar la interfaz después del movimiento de la IA
               (when (verificar-victoria board "O")  ; Verificar si la IA ha ganado
                 (message-box "Fin del juego" "La IA ha ganado.")))])))
      ; Verificar nuevamente si el tablero está lleno después del movimiento de la IA
      (when (tablero-lleno? board)
        (message-box "Fin del juego" "¡Empate!")))
    
    ; Crear la cuadrícula de botones para representar el tablero
    ; Cada botón se enlaza a su respectiva función de clic
    (for ([i (in-range rows)])
      (let ([row-panel (new horizontal-panel% [parent grid-panel] [alignment '(center center)])])
        (for ([j (in-range cols)])
          (let ([btn (new button% 
                          [parent row-panel]
                          [label ""]  ; Inicialmente, el botón está vacío
                          [min-width 40]
                          [min-height 40]
                          [stretchable-width #t]  ; Permite ajustar el tamaño del botón
                          [stretchable-height #t]
                          [callback (λ (b e) (button-click (+ (* i cols) j)))])])  ; Asocia el clic del botón a la posición (i, j)
            (vector-set! buttons (+ (* i cols) j) btn)))))  ; Almacena el botón en el vector de botones
    
    ; Botón para reiniciar el juego, reinicia el tablero y actualiza la interfaz gráfica
    (new button% 
         [parent panel]
         [label "Reiniciar"]
         [callback (λ (b e)
                     (set! board (crear-tablero rows cols))  ; Crea un nuevo tablero vacío
                     (update-gui))])))  ; Actualiza la interfaz para reflejar el reinicio

; Función principal para iniciar el juego
; Verifica si el tamaño del tablero es válido y crea una nueva ventana de juego
(define (TTT m n)
  (if (and (>= m 3) (<= m 10) (>= n 3) (<= n 10))  ; Verifica que el tamaño del tablero esté entre 3x3 y 10x10
      (let ([game-window (new tictactoe-frame% [rows m] [cols n])])
        (send game-window show #t)  ; Muestra la ventana del juego
        game-window)
      (error "Tamaño de tablero inválido. Por favor, usa dimensiones entre 3x3 y 10x10.")))  ; Error si el tamaño es inválido