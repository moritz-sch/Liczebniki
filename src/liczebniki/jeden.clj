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


(ns liczebniki.jeden
  (:require [liczebniki.outputstyle :refer [printHeadline printBigHeadline printTableLine]]))

(defn printJedenCardinalColl
  "The declension table for 1 as a cardinal number is hard-coded here as an output function as it is completely irregular, also the information that no collective number 1 exists"
  []
  (printBigHeadline "Cardinal numbers (liczebniki główne)")
  (printHeadline "Singular (liczba pojedyńcza)")
  (printHeadline "Masculine Personal, Masculine Animate")
  (printTableLine :mianownik "jeden")
  (printTableLine :dopełniacz "jednego")
  (printTableLine :celownik "jednemu")
  (printTableLine :biernik "jednego")
  (printTableLine :narzędnik "jednym")
  (printTableLine :miejscownik "jednym")
  (printTableLine :wołacz "jeden")
  (newline)
  (newline)
  (printHeadline "Masculine Inanimate")
  (printTableLine :mianownik "jeden")
  (printTableLine :dopełniacz "jednego")
  (printTableLine :celownik "jednemu")
  (printTableLine :biernik "jeden")
  (printTableLine :narzędnik "jednym")
  (printTableLine :miejscownik "jednym")
  (printTableLine :wołacz "jeden")
  (newline)
  (newline)
  (printHeadline "Feminine")
  (printTableLine :mianownik "jedna")
  (printTableLine :dopełniacz "jednej")
  (printTableLine :celownik "jednej")
  (printTableLine :biernik "jedną")
  (printTableLine :narzędnik "jedną")
  (printTableLine :miejscownik "jednej")
  (printTableLine :wołacz "jedna")
  (newline)
  (newline)
  (printHeadline "Neutral")
  (printTableLine :mianownik "jedno")
  (printTableLine :dopełniacz "jednego")
  (printTableLine :celownik "jednemu")
  (printTableLine :biernik "jedno")
  (printTableLine :narzędnik "jednym")
  (printTableLine :miejscownik "jednym")
  (printTableLine :wołacz "jedno")
  (newline)
  (newline)
  (printHeadline "Plural (liczba mnoga)")
  (printHeadline "Masculine Personal")
  (printTableLine :mianownik "jedni")
  (printTableLine :dopełniacz "jednych")
  (printTableLine :celownik "jednym")
  (printTableLine :biernik "jednych")
  (printTableLine :narzędnik "jednymi")
  (printTableLine :miejscownik "jednych")
  (printTableLine :wołacz "jedni")
  (newline)
  (newline)
  (printHeadline "Non Masculine Personal")
  (printTableLine :mianownik "jedne")
  (printTableLine :dopełniacz "jednych")
  (printTableLine :celownik "jednym")
  (printTableLine :biernik "jedne")
  (printTableLine :narzędnik "jednymi")
  (printTableLine :miejscownik "jednych")
  (printTableLine :wołacz "jedne")
  (newline)
  (newline)
  (printBigHeadline "Collective numbers (liczebniki zbiorowe)")
  (print "N/A")
  (newline)
  (newline)
  (newline))
