(defun row-string (moves row)
  (concat
    " "
    (aref moves (+ 0 (* 3 (- row 1))))
    " | "
    (aref moves (+ 1 (* 3 (- row 1))))
    " | "
    (aref moves (+ 2 (* 3 (- row 1))))))

(defun horizontal-line ()
  "-----------")


(defun print-board (moves)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (row-string moves 1))
    (newline)
    (insert (horizontal-line))
    (newline)
    (insert (row-string moves 2))
    (newline)
    (insert (horizontal-line))
    (newline)
    (insert (row-string moves 3))))

(defun horizontal-winner (moves)
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
  (or
    (horizontal-winner moves)
    (vertical-winner moves)
    (diagonal-winner moves)))

(defun go-to-ttt-buffer ()
  (switch-to-buffer-other-window "*Tic Tac Toe*")
  (compilation-mode)
  (erase-buffer))

(defun alternate-current-player-token (current)
  (if (string-equal current "X") "O" "X"))

(defun display-winner (winner)
  (newline) (newline) (newline)
  (insert
    (concat (if (string-equal winner "X") "Player" "Computer") " wins!")))

(defun get-player-move (moves token)
  (message (concat "TOKEN: " token))
  (if (string-equal token "X")
    (let* ((player-choice -1)
            (empty-spaces (empty-spaces moves))
            (empty-spaces-string (mapconcat 'number-to-string (mapcar (lambda (i) (+ 1 i)) empty-spaces) ", ")))
      (while
        (not (member player-choice empty-spaces))
        (setq player-choice
          (- (read-number (concat "Where would you like to place your marker? (" empty-spaces-string ") ")) 1))
        (when
          (not (member player-choice empty-spaces))
          (newline) (newline) (newline)
          (insert (concat
                    "That is not a valid choice. Valid choices are: " empty-spaces-string))))
      player-choice)
    (computer-choice moves)))

(defun computer-choice (moves)
  (let ((choices (empty-spaces moves)))
    (nth (random (length choices)) choices)))

(defun empty-spaces (moves)
  (let ((spaces '()))
    (dotimes (i 9)
      (when
        (string-equal (aref moves i) " ")
        (setq spaces (cons i spaces))))
    (reverse spaces)))

(defun play-game ()
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
    (display-winner (winner moves))))

(play-game)
