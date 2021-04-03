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


(ns liczebniki.maps.createmapsnoun)

(defn createSmallerThan20MapNouns
  "Returns a map with the declensions of the numbers 1 to 19 as nouns"
  []
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]
        suffixes-lp ["ka" "ki" "ce" "kę" "ką" "ce" "ko"]
        suffixes-lm ["ki" "ek" "kom" "ki" "kami" "kach" "ki"]]
    (loop [prefixes ["jedyn" "dwój" "trój" "czwór" "piąt" "szóst" "siódem" "ósem" "dziewiąt" "dziesiąt" "jedenast" "dwunast" "trzynast" "czternast" "piętnast" "szesnast" "siedemnast" "osiemnast" "dziewiętnast"]
           zs {}
           i  1]
      (if (empty? prefixes)
        zs
        (let [cslp (zipmap cases (map #(str (first prefixes) %) suffixes-lp))
              cslm (zipmap cases (map #(str (first prefixes) %) suffixes-lm))]
          (recur (rest prefixes) (assoc-in (assoc-in zs [i :lp] cslp) [i :lm] cslm) (inc i)))))))

(defn add20to100toMap
  "Adds the declensions of the numbers 20, 30.. 100 as nouns to a map"
  [ms]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]
        suffixes-lp ["ka" "ki" "ce" "kę" "ką" "ce" "ko"]
        suffixes-lm ["ki" "ek" "kom" "ki" "kami" "kach" "ki"]]
    (loop [prefixes ["dwudziest" "trzydziest" "czterdziest" "pięćdziest" "sześćdziest" "siedemdziest" "osiemdziest" "dziewięćdziest" "set"]
           zs ms
           i  2]
      (if (empty? prefixes)
        zs
        (let [cslp (zipmap cases (map #(str (first prefixes) %) suffixes-lp))
              cslm (zipmap cases (map #(str (first prefixes) %) suffixes-lm))]
          (recur (rest prefixes) (assoc-in (assoc-in zs [(* i 10) :lp] cslp) [(* i 10) :lm] cslm) (inc i)))))))

;define the map
(def nounMap (add20to100toMap (createSmallerThan20MapNouns)))
