;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <https://www.gnu.org/licenses/>.


(ns liczebniki.outputstyle)

(def ansiStyles
  {:bold   "[1m"
   :italic "[2m"
   :red    "[31m"
   :green  "[32m"
   :orange "[33m"
   :blue   "[34m"
   :purple "[35m"
   :teal   "[36m"
   ;reset = white
   :reset  "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansiStyles)))

(defn style
  "Applies a style to a string"
  [text color]
  (str (ansi color) text (ansi :reset)))

  
(def caseNameMapFormatted
  {:mianownik    "Mianownik     "
   :dopełniacz   "Dopełniacz    "
   :celownik     "Celownik      "
   :biernik      "Biernik       "
   :narzędnik    "Narzędnik     "
   :miejscownik  "Miejscownik   "
   :wołacz       "Wołacz        "})

(def caseNameMap
  {:mianownik "mianownik"
   :dopełniacz "dopełniacz"
   :celownik "celownik"
   :biernik "biernik"
   :narzędnik "narzędnik"
   :miejscownik "miejscownik"
   :wołacz "wołacz"})

(defn printHeadline
  [text]
  (print (style text :purple))
  (newline)
  (print (style (apply str (repeat (count text) "=")) :purple))
  (newline)
  (newline))

(defn printBigHeadline
  [text]
  (let [len (count text)]
    (print (style (apply str (repeat len "-")) :purple))
    (newline)
    (print (style text :purple))
    (newline)
    (print (style (apply str (repeat len "-")) :purple))
    (newline)
    (newline)))

(defn printTableLine
  "Prints the declension table line for the case c"
  [c form]
  (print (style (caseNameMapFormatted c) :purple))
  (print (style form :blue))
  (print (style " (+" :teal))
  (print (style (caseNameMap c) :teal))
  (print (style ")" :teal))
  (newline))
