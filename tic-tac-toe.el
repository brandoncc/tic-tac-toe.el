(defun row-string (moves row)
  "Return a ROW string using elements from MOVES."
  (concat
    " "
    (aref moves (+ 0 (* 3 (- row 1))))
    " | "
    (aref moves (+ 1 (* 3 (- row 1))))
    " | "
    (aref moves (+ 2 (* 3 (- row 1))))))

(defun horizontal-line ()
  "Return a horizontal section for the board."
  "-----------")


(defun print-board (moves)
  "Print the board to the current (should be the game) buffer."
  (erase-buffer)
  (insert (row-string moves 1))
  (newline)
  (insert (horizontal-line))
  (newline)
  (insert (row-string moves 2))
  (newline)
  (insert (horizontal-line))
  (newline)
  (insert (row-string moves 3)))

(defun horizontal-winner (moves)
  "Check for a winner in any row."
  (cond
    ((and
       (not (string-equal (aref moves 0) " "))
       (string-equal (aref moves 0) (aref moves 1))
       (string-equal (aref moves 0) (aref moves 2)))
      (aref moves 0))
    ((and
       (not (string-equal (aref moves 3) " "))
       (string-equal (aref moves 3) (aref moves 4))
       (string-equal (aref moves 3) (aref moves 5)))
      (aref moves 3))
    ((and
       (not (string-equal (aref moves 6) " "))
       (string-equal (aref moves 6) (aref moves 7))
       (string-equal (aref moves 6) (aref moves 8)))
      (aref moves 6))
    (t nil)))

(defun vertical-winner (moves)
  "Check for a winner in any column."
  (cond
    ((and
       (not (string-equal (aref moves 0) " "))
       (string-equal (aref moves 0) (aref moves 3))
       (string-equal (aref moves 0) (aref moves 6)))
      (aref moves 0))
    ((and
       (not (string-equal (aref moves 1) " "))
       (string-equal (aref moves 1) (aref moves 4))
       (string-equal (aref moves 1) (aref moves 7)))
      (aref moves 1))
    ((and
       (not (string-equal (aref moves 2) " "))
       (string-equal (aref moves 2) (aref moves 5))
       (string-equal (aref moves 2) (aref moves 8)))
      (aref moves 2))
    (t nil)))

(defun diagonal-winner (moves)
  "Check for a winner in any diagonal."
  (cond
    ((and
       (not (string-equal (aref moves 0) " "))
       (string-equal (aref moves 0) (aref moves 4))
       (string-equal (aref moves 0) (aref moves 8)))
      (aref moves 0))
    ((and
       (not (string-equal (aref moves 2) " "))
       (string-equal (aref moves 2) (aref moves 4))
       (string-equal (aref moves 2) (aref moves 6)))
      (aref moves 2))
    (t nil)))

(defun winner (moves)
  "Return any winner from the board."
  (or
    (horizontal-winner moves)
    (vertical-winner moves)
    (diagonal-winner moves)))

(defun go-to-ttt-buffer ()
  "Create and select the game buffer and window."
  (unless (string-equal (buffer-name) "*Tic Tac Toe*")
    (switch-to-buffer-other-window "*Tic Tac Toe*"))
  (compilation-mode)
  (setq inhibit-read-only t)
  (erase-buffer))

(defun alternate-current-player-token (current)
  "If the CURRENT token is X, return O, otherwise return X."
  (if (string-equal current "X") "O" "X"))

(defun display-winner (winner)
  "Display the winner in the current (should be the game) buffer."
  (newline) (newline) (newline)
  (insert
    (concat (if (string-equal winner "X") "Player" "Computer") " wins!")))

(defun get-player-move (moves token)
  "If it is the player's turn, ask for a move. Otherwise return computer's choice."
  (if (string-equal token "X")
    (let* ((player-choice nil)
            (empty-spaces (empty-spaces moves))
            ;; Create a string containing all empty spaces, joined by ", ".
            (empty-spaces-string
              ;; The spaces are numbers, so we have to convert each one to a
              ;; string.
              (mapconcat 'number-to-string
                ;; Since the spaces are zero-indexed, we need to increment them
                ;; to show the player the correct space. This is also where we
                ;; join them with ", ".
                (mapcar (lambda (i) (+ 1 i)) empty-spaces) ", ")))

      (while
        ;; Loop while the player's choice is not available.
        (not (member player-choice empty-spaces))

        ;; Get a new choice from the player.
        (setq player-choice
          ;; To adjust for the zero-based position index, we need to subtract 1
          ;; from the player' selection.
          (-
            (read-number
              (concat
                "Where would you like to place your marker? ("
                empty-spaces-string
                ") "))
            1))

        ;; If the player selects an invalid position, display a message in the
        ;; buffer saying so.
        (when
          (not (member player-choice empty-spaces))
          (newline) (newline) (newline)
          (insert (concat
                    "That is not a valid choice. Valid choices are: " empty-spaces-string))))

      ;; Return the player's choice.
      player-choice)
    (computer-choice moves)))

(defun computer-choice (moves)
  "Return a random selection for the computer."
  (let ((choices (empty-spaces moves)))
    ;; Get the element at the chosen index.
    (nth
      ;; Choose a random number between 0 (inclusive) and the length of the array
      ;; (exclusive).
      (random (length choices))

      ;; Use that random number to access this array
      choices)))

(defun empty-spaces (moves)
  "Get a list containing all of the empty board position indexes."
  (let ((spaces '()))
    ;; For each board index, run the given code.
    (dotimes (i 9)
      (when
        ;; If the board position is empty,
        (string-equal (aref moves i) " ")

        ;; Then add it to the list which will be returned.
        (setq spaces (cons i spaces))))

    ;; The elements are added in reverse order. We reverse them here so that
    ;; when we display them, they are in ascending order.
    (reverse spaces)))

(defun play-game ()
  "Play one round of Tic Tac Toe."
  (interactive)
  (let ((current-player-token "")
         (moves [" " " " " " " " " " " " " " " " " "]))
    (go-to-ttt-buffer)
    (while
      (and
        (not (winner moves))
        (> (length (empty-spaces moves)) 0))
      (setq current-player-token
        (alternate-current-player-token current-player-token))
      (print-board moves)
      (let ((player-move (get-player-move moves current-player-token)))
        (aset moves player-move current-player-token)))
    (print-board moves)
    (display-winner (winner moves))

    ;; We must reset the board because the object in memory lives on for some
    ;; reason and affects the next time this function is called.
    (reset-board moves)))

(defun reset-board (board)
  "Reset the board to empty strings."
  (fillarray board " "))

(defun play-again? ()
  "Ask if the player would like to play again and return the response."
  (let ((response nil))
    (while (not (member response '("Y" "y" "N" "n")))
      (setq response (read-string "Play again? (y/n) ")))
    (downcase response)))

(let ((continue-playing "y"))
  (while (string-equal continue-playing "y")
    (play-game)
    (setq continue-playing (play-again?)))
  (quit-window t (get-buffer-window "*Tic Tac Toe*")))
