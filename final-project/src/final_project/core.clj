

;FLOWCHART FOR MY SUDOKU SOLVER

;Define the function build_board which takes a single argument board.
;Use map to call partition on board, which splits board into smaller sequences.
;Define the function print_board which takes a board as an argument.
;Use let to define row_separator as a string of 37 dashes.
;Use println and dotimes to print each row of the board.
;Use doseq to print each cell of the row.
;Use mod and when to insert a separator between sub-grids of the board.
;Define the function index which takes a single argument collection.
;Use cond to check the type of collection and return a sequence based on the type.
;Define the functions rows, row_for, column_for, and subgrid_for which extract rows, columns, and subgrids of the Sudoku board.
;Define the function numbers_present_for which returns the set of numbers present in a row, column, or subgrid of the board.
;Define the function possible_placements which takes a board and an index and returns the set of numbers that can be placed at that index on the board.

;*********************************************************************************************************************************************


(ns final-project.core
  (:require [clojure.set :as set]))

(def board1 
  '[- - 4 - - 3 - - 1
    8 - - 9 - 5 - - -
    - - - - - - - - -
    - 7 - - 4 - 5 - -
    - - - 8 - - - 6 -
    9 - - 2 - - - 3 -
    - 1 2 - - - - - 7
    - 4 - - - - 8 - -
    - 5 - 7 - - - 9 2])
;;^will be my board

(defn build_board [board] ;;function build_board will take in arg 'board'
  (map #(partition 3 %) ;; map function will call partition which will split the seq into a smaller seq
       (partition 9 board)))


(defn print_board [board] ;; function will take in arg board 
  (let [row_separator (apply str (repeat 37 "-"))] ;; creating variable row_separator which will use the 'apply' function on 'str' function 
    (println row_separator) ;;will print out row_separator
    (dotimes [row (count board)] ;; 'dotimes' function will iterate through board and and will print a  '|' to 
      (print "| ")
      (doseq [new_row (nth board row)] ;;return value at index
        (doseq [cell (butlast new_row)] ;;return seq but last new_row
          (print (str cell "   "))) ;; will print a cell
        (print (str (last new_row) " | "))) ;;print new row
      (println)
      (when (zero? (mod (inc row) 3)) ;; check the value of row
        (println row_separator))))) ;;will print '-'

;;print unsolved board
(-> board1 ;;will pass function to build_board
    build_board ;;here build_board will pass to print_board
    print_board) ;; will print the board :)

(defn index [collection] ;; function index will take arg collection 
  (cond ;; checks type of collection arg
    (map? collection) (seq collection) ;; if collection is map return sequence 
    (set? collection) (map vector collection collection) ;; if collection is a set-> map will create a sequence of index-value pairs
    :else (map vector (iterate inc 0) collection))) ;; if it is a different type of collection map will create a sequence of index-value pairs


(defn rows [board size] ;; num of rows to be extracted from board
  (partition size board)) ;; split board by num of rows

(defn row_for [board index size]
  (nth (rows board size) 
       (/ index 9)))

(row_for board1 1 9) ;; call function row_for take in board1 


(defn column_for [board index size]
  (let [columns (mod index size)]
    (map #(nth % columns)
         (rows board size))))

(column_for board1 2 9)


(defn subgrid_for [board i]
  (let [rows (rows board 9)
        subgrid_col (/ (mod i 9) 3)
        subgrid_row (/ (/ i 9) 3)
        grp_col (column_for (mapcat #(partition 3 %) rows) subgrid_col 3)
        grp (take 3 (drop (* 3 (int subgrid_row)) grp_col))]
    (flatten grp)))

(subgrid_for board1 0)


(defn numbers_present_for [board i]
  (set 
   (concat(row_for board i 9) 
    (column_for board i 9) 
    (subgrid_for board i))))

(numbers_present_for board1 1)

(numbers_present_for (assoc board1 1 8) 1)


(defn possible_placements [board index] ;;function should check if there is a repeating number in row and column by checking board 1-9
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers_present_for board index)))

(defn position_of [element collection] ;; function position will take 2 args : elements and collection 
  (for [[i j] (index collection) ;; for loop to iterate through elements in collection.. call index function which will create a sequence of index-value pairs for the elements in the collection
        :when (= element j)] i)) ;; will only return elements which are equal to elements 

(defn solve_board [board]
  (if-let [[i & _]
           (and (some '#{0} board) ;;check for '-' on board 
                (position_of '#{0} board))]
    (flatten (map #(solve_board (assoc board i %))
                  (possible_placements board i)))
    board))

;;print solved board 
(-> board1
    solve_board
    build_board
    print_board)
