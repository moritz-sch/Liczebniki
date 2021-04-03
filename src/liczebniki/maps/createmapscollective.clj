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


(ns liczebniki.maps.createmapscollective)

;numbers 1 to 3
(def smallerThan4MapCollective
                      {1  {:mianownik "jeden" :dopełniacz "jeden" :celownik "jeden" :biernik "jeden" :narzędnik "jeden" :miejscownik "jeden" :wołacz "jeden"}
                       
                       2  {:mianownik "dwoje" :dopełniacz "dwojga" :celownik "dwojgu" :biernik "dwoje" :narzędnik "dwojgiem" :miejscownik "dwojgu" :wołacz "dwoje"}
                       
                       3  {:mianownik "troje" :dopełniacz "trojga" :celownik "trojgu" :biernik "troje" :narzędnik "trojgiem" :miejscownik "trojgu" :wołacz "troje"}
                      })
                      
(defn add5to19toMap
  "Adds all declensions of the numbers 5 to 10 to a map"
  [ms]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]
        suffixes ["ro" "rga" "rgu" "ro" "rgiem" "rgu" "ro"]]
    (loop [prefixes ["czwo" "pięcio" "sześcio" "siedmio" "ośmio" "dziewięcio" "dziesięcio" "jedenaścio" "dwanaścio" "trzynaścio" "czternaścio" "piętnaścio" "szesnaścio" "siedemnaścio" "osiemnaścio" "dziewiętnaścio"]
           zs ms
           i  4]
      (if (empty? prefixes)
        zs
        (let [cs (zipmap cases (map #(str (first prefixes) %) suffixes))]
          (recur (rest prefixes) (assoc zs i cs) (inc i)))))))

;define map with the collective numbers smaller than 20
(def smallerThan20MapCollective (add5to19toMap smallerThan4MapCollective))



(defn createTensMapCollective
  "Adds all declensions of the numbers 20, 30, ... 90 to a map"
  []
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]
        suffixes ["dzieścioro" "dzieściorga" "dzieściorgu" "dzieścioro" "dzieściorgiem" "dzieściorgu" "dzieścioro"]]
    (loop [prefixes ["dwa" "trzy" "czter" "pięć" "sześć" "siedem" "osiem" "dziewięć"]
           zs {}
           i  2]
      (if (empty? prefixes)
        zs
        (let [cs (zipmap cases (map #(str (first prefixes) %) suffixes))]
          (recur (rest prefixes) (assoc zs i cs) (inc i)))))))

;define map with the collective numbers 20, 30, 40... 90
(def tensMapCollective (createTensMapCollective))
