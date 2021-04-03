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


(ns liczebniki.maps.createmapscardinal)

;the functions in this file create maps which store all cases in all genera for the numbers 2-19, 20, 30.. 90, 100, 200.. 900, 10^3, 10^6, 10^9...

;numbers 1 to 12
(def smallerThan13Map {
                       ;for one this is not the actual declension! Since one is a special case, it is handled differently, this is just to be used as a part of numerals
                       1  {
                           :mp {:mianownik "jeden" :dopełniacz "jeden" :celownik "jeden" :biernik "jeden" :narzędnik "jeden" :miejscownik "jeden"}
                           
                           :f {:mianownik "jeden" :dopełniacz "jeden" :celownik "jeden" :biernik "jeden" :narzędnik "jeden" :miejscownik "jeden"}
                           
                           :nonmp {:mianownik "jeden" :dopełniacz "jeden" :celownik "jeden" :biernik "jeden" :narzędnik "jeden" :miejscownik "jeden"}
                          }
                       
                       2  {
                           :mp {:mianownik "dwóch" :dopełniacz {1 "dwu" 2 "dwóch"}      :celownik "dwom" :biernik "dwóch" :narzędnik "dwoma" :miejscownik {1 "dwu" 2 "dwóch"}}
                           
                           :f {:mianownik "dwie" :dopełniacz {1 "dwu" 2 "dwóch"} :celownik "dwom" :biernik "dwie" :narzędnik {1 "dwoma" 2 "dwiema"} :miejscownik "dwóch"}
                           
                           :nonmp {:mianownik "dwa" :dopełniacz {1 "dwu" 2 "dwóch"} :celownik "dwom" :biernik "dwa" :narzędnik "dwoma" :miejscownik {1 "dwu" 2 "dwóch"}}
                          }
                          
                       3  {
                           :mp {:mianownik "trzech" :dopełniacz "trzech" :celownik "trzem" :biernik "trzech" :narzędnik "trzema" :miejscownik "trzech"}
                           
                           :f {:mianownik "trzy" :dopełniacz "trzech" :celownik "trzem" :biernik "trzy" :narzędnik "trzema" :miejscownik "trzech"}
                           
                           :nonmp {:mianownik "trzy" :dopełniacz "trzech" :celownik "trzem" :biernik "trzy" :narzędnik "trzema" :miejscownik "trzech"}
                          }
                          
                       4  {
                           :mp {:mianownik "czterech" :dopełniacz "czterech" :celownik "czterem" :biernik "czterech" :narzędnik "czterema" :miejscownik "czterech"}
                           
                           :f {:mianownik "cztery" :dopełniacz "czterech" :celownik "czterem" :biernik "cztery" :narzędnik "czterema" :miejscownik "czterech"}
                           
                           :nonmp {:mianownik "cztery" :dopełniacz "czterech" :celownik "czterem" :biernik "cztery" :narzędnik "czterema" :miejscownik "czterech"}
                          }
                          
                       5  {
                           :mp {:mianownik "pięciu" :dopełniacz "pięciu" :celownik "pięciu" :biernik "pięciu" :narzędnik {1 "pięcioma" 2 "pięciu"} :miejscownik "pięciu"}
                           
                           :f {:mianownik "pięć" :dopełniacz "pięciu" :celownik "pięciu" :biernik "pięć" :narzędnik {1 "pięcioma" 2 "pięciu"} :miejscownik "pięciu"}
                           
                           :nonmp {:mianownik "pięć" :dopełniacz "pięciu" :celownik "pięciu" :biernik "pięć" :narzędnik {1 "pięcioma" 2 "pięciu"} :miejscownik "pięciu"}
                          }
                          
                       6  {
                           :mp {:mianownik "sześciu" :dopełniacz "sześciu" :celownik "sześciu" :biernik "sześciu" :narzędnik {1 "sześcioma" 2 "sześciu"} :miejscownik "sześciu"}
                           
                           :f {:mianownik "sześć" :dopełniacz "sześciu" :celownik "sześciu" :biernik "sześć" :narzędnik {1 "sześcioma" 2 "sześciu"} :miejscownik "sześciu"}
                           
                           :nonmp {:mianownik "sześć" :dopełniacz "sześciu" :celownik "sześciu" :biernik "sześć" :narzędnik {1 "sześcioma" 2 "sześciu"} :miejscownik "sześciu"}
                          }
                          
                       7  {
                           :mp {:mianownik "siedmiu" :dopełniacz "siedmiu" :celownik "siedmiu" :biernik "siedmiu" :narzędnik {1 "siedmioma" 2 "siedmiu"} :miejscownik "siedmiu"}
                           
                           :f {:mianownik "siedem" :dopełniacz "siedmiu" :celownik "siedmiu" :biernik "siedem" :narzędnik {1 "siedmioma" 2 "siedmiu"} :miejscownik "siedmiu"}
                           
                           :nonmp {:mianownik "siedem" :dopełniacz "siedmiu" :celownik "siedmiu" :biernik "siedem" :narzędnik {1 "siedmioma" 2 "siedmiu"} :miejscownik "siedmiu"}
                          }
                          
                       8  {
                           :mp {:mianownik "ośmiu" :dopełniacz "ośmiu" :celownik "ośmiu" :biernik "ośmiu" :narzędnik {1 "ośmioma" 2 "ośmiu"} :miejscownik "ośmiu"}
                           
                           :f {:mianownik "osiem" :dopełniacz "ośmiu" :celownik "ośmiu" :biernik "osiem" :narzędnik {1 "ośmioma" 2 "ośmiu"} :miejscownik "ośmiu"}
                           
                           :nonmp {:mianownik "osiem" :dopełniacz "ośmiu" :celownik "ośmiu" :biernik "osiem" :narzędnik {1 "ośmioma" 2 "ośmiu"} :miejscownik "ośmiu"}
                          }
                          
                       9  {
                           :mp {:mianownik "dziewięciu" :dopełniacz "dziewięciu" :celownik "dziewięciu" :biernik "dziewięciu" :narzędnik {1 "dziewięcioma" 2 "dziewięciu"} :miejscownik "dziewięciu"}
                           
                           :f {:mianownik "dziewięć" :dopełniacz "dziewięciu" :celownik "dziewięciu" :biernik "dziewięć" :narzędnik {1 "dziewięcioma" 2 "dziewięciu"} :miejscownik "dziewięciu"}
                           
                           :nonmp {:mianownik "dziewięć" :dopełniacz "dziewięciu" :celownik "dziewięciu" :biernik "dziewięć" :narzędnik {1 "dziewięcioma" 2 "dziewięciu"} :miejscownik "dziewięciu"}
                          }
                          
                       10 {
                           :mp {:mianownik "dziesięciu" :dopełniacz "dziesięciu" :celownik "dziesięciu" :biernik "dziesięciu" :narzędnik {1 "dziesięcioma" 2 "dziesięciu"} :miejscownik "dziesięciu"}
                           
                           :f {:mianownik "dziesięć" :dopełniacz "dziesięciu" :celownik "dziesięciu" :biernik "dziesięć" :narzędnik {1 "dziesięcioma" 2 "dziesięciu"} :miejscownik "dziesięciu"}
                           
                           :nonmp {:mianownik "dziesięć" :dopełniacz "dziesięciu" :celownik "dziesięciu" :biernik "dziesięć" :narzędnik {1 "dziesięcioma" 2 "dziesięciu"} :miejscownik "dziesięciu"}
                          }
                          
                       11 {
                           :mp {:mianownik "jedenastu" :dopełniacz "jedenastu" :celownik "jedenastu" :biernik "jedenastu" :narzędnik {1 "jedenastoma" 2 "jedenastu"} :miejscownik "jedenastu"}
                           
                           :f {:mianownik "jedenaście" :dopełniacz "jedenastu" :celownik "jedenastu" :biernik "jedenaście" :narzędnik {1 "jedenastoma" 2 "jedenastu"} :miejscownik "jedenastu"}
                           
                           :nonmp {:mianownik "jedenaście" :dopełniacz "jedenastu" :celownik "jedenastu" :biernik "jedenaście" :narzędnik {1 "jedenastoma" 2 "jedenastu"} :miejscownik "jedenastu"}
                          }   
                          
                       12 {
                           :mp {:mianownik "dwunastu" :dopełniacz "dwunastu" :celownik "dwunastu" :biernik "dwunastu" :narzędnik {1 "dwunastoma" 2 "dwunastu"} :miejscownik "dwunastu"}
                           
                           :f {:mianownik "dwanaście" :dopełniacz "dwunastu" :celownik "dwunastu" :biernik "dwanaście" :narzędnik {1 "dwunastoma" 2 "dwunastu"} :miejscownik "dwunastu"}
                           
                           :nonmp {:mianownik "dwanaście" :dopełniacz "dwunastu" :celownik "dwunastu" :biernik "dwanaście" :narzędnik {1 "dwunastoma" 2 "dwunastu"} :miejscownik "dwunastu"}
                          }})
                      

(defn strIntoMap
  "If s1 and s2 are strings, the function just concatenates them, if s2 is a map whose values are strings, the function returns a map where all values are concatenations of s1 with the values of s2"
  [s1 s2]
  (if (string? s2)
    (str s1 s2)
    (apply hash-map (flatten (for [x s2] [(first x) (str s1 (second x))])))))

(defn add13to19toMap
  "Adds the declensions of the numbers 13 to 19 to a map"
  [ms]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik]
        suffixes-mp ["nastu" "nastu" "nastu" "nastu" {1 "nastoma" 2 "nastu"} "nastu"]
        suffixes-nonmp ["naście" "nastu" "nastu" "naście" {1 "nastoma" 2 "nastu"} "nastu"]]
    (loop [prefixes ["trzy" "czter" "pięt" "szes" "siedem" "osiem" "dziewięt"]
           zs ms
           i 13]
      (if (empty? prefixes)
        zs
        (let [cases-mp (zipmap cases (map #(strIntoMap (first prefixes) %) suffixes-mp))
              cases-nonmp (zipmap cases (map #(strIntoMap (first prefixes) %) suffixes-nonmp))]
          (recur (rest prefixes) (assoc zs i {:mp cases-mp :f cases-nonmp :nonmp cases-nonmp}) (inc i)))))))

;complete the map containing the numbers below 20
(def smallerThan20Map (add13to19toMap smallerThan13Map))



;numbers 20, 30, 40
(def to40Map {2 {
                 :mp {:mianownik "dwudziestu" :dopełniacz "dwudziestu" :celownik "dwudziestu" :biernik "dwudziestu" :narzędnik {1 "dwudziestoma" 2 "dwudziestu"} :miejscownik "dwudziestu"}
                 
                 :f {:mianownik "dwadzieścia" :dopełniacz "dwudziestu" :celownik "dwudziestu" :biernik "dwadzieścia" :narzędnik {1 "dwudziestoma" 2 "dwudziestu"} :miejscownik "dwudziestu"}
                 
                 :nonmp {:mianownik "dwadzieścia" :dopełniacz "dwudziestu" :celownik "dwudziestu" :biernik "dwadzieścia" :narzędnik {1 "dwudziestoma" 2 "dwudziestu"} :miejscownik "dwudziestu"}
                }
                
             3 {
                 :mp {:mianownik "trzydziestu" :dopełniacz "trzydziestu" :celownik "trzydziestu" :biernik "trzydziestu" :narzędnik {1 "trzydziestoma" 2 "trzydziestu"} :miejscownik "trzydziestu"}
                 
                 :f {:mianownik "trzydzieści" :dopełniacz "trzydziestu" :celownik "trzydziestu" :biernik "trzydzieści" :narzędnik {1 "trzydziestoma" 2 "trzydziestu"} :miejscownik "trzydziestu"}
                 
                 :nonmp {:mianownik "trzydzieści" :dopełniacz "trzydziestu" :celownik "trzydziestu" :biernik "trzydzieści" :narzędnik {1 "trzydziestoma" 2 "trzydziestu"} :miejscownik "trzydziestu"}
                }
                
             4 {
                 :mp {:mianownik "czterdziestu" :dopełniacz "czterdziestu" :celownik "czterdziestu" :biernik "czterdziestu" :narzędnik {1 "czterdziestoma" 2 "czterdziestu"} :miejscownik "czterdziestu"}
                 
                 :f {:mianownik "czterdzieści" :dopełniacz "czterdziestu" :celownik "czterdziestu" :biernik "czterdzieśći" :narzędnik {1 "czterdziestoma" 2 "czterdziestu"} :miejscownik "czterdziestu"}
                 
                 :nonmp {:mianownik "czterdzieści" :dopełniacz "czterdziestu" :celownik "czterdziestu" :biernik "czterdzieśći" :narzędnik {1 "czterdziestoma" 2 "czterdziestu"} :miejscownik "czterdziestu"}
                }
              })
              
(defn add50to90toMap
  "Adds the cases of the multiples of 10 from 50 to 90 to a map"
  [ms]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik]
        suffixes-mp ["dziesięciu" "dziesięciu" "dziesięciu" "dziesięciu" {1 "dziesięcioma" 2 "dziesięciu"} "dziesięciu"]
        suffixes-nonmp ["dziesiąt" "dziesięciu" "dziesięciu" "dziesiąt" {1 "dziesięcioma" 2 "dziesięciu"} "dziesięciu"]]
    (loop [prefixes ["pięć" "sześć" "siedem" "osiem" "dziewięć"]
           zs ms
           i 5]
      (if (empty? prefixes)
        zs
        (let [cases-mp (zipmap cases (map #(strIntoMap (first prefixes) %) suffixes-mp))
              cases-nonmp (zipmap cases (map #(strIntoMap (first prefixes) %) suffixes-nonmp))]
          (recur (rest prefixes) (assoc zs i {:mp cases-mp :f cases-nonmp :nonmp cases-nonmp}) (inc i)))))))

;complete the map of tens
(def tensMap (add50to90toMap to40Map))



(defn createHundredsMap
  "Returns a map with the declensions of 100, 200.. 900"
  []
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik]
        names [["sto" "stu"] ["dwieśćie" "dwustu"] ["trysta" "trzystu"] ["czterysta" "czterystu"] ["pięćset" "pięciuset"] [ "sześćset" "sześciuset"] ["siedemset" "siedmiuset"] ["osiemset" "ośmiuset"] ["dziewięćset" "dziewięciuset"]]]
    (loop [nns names
           zs {}
           i 1]
      (if (empty? nns)
        zs
        (let [fnns (first nns)
              mian (first fnns)
              dop  (second fnns)
              cases-mp (zipmap cases (repeat 6 dop))
              cases-nonmp (zipmap cases [mian dop dop mian dop dop])]
          (recur (rest nns) (assoc zs i {:mp cases-mp :f cases-nonmp :nonmp cases-nonmp}) (inc i)))))))

;creates the map of hundreds
(def hundredsMap (createHundredsMap))

              
              
(defn createLargeNumbersMap
  "Returns a map with declensions of the powers of 1000 up to 10^78, the key is always the exponent divided by 3 (so 1 for thousand, 2 for a million and so on)"
  []
  (let [as {0 {
               :lp {:mianownik "jeden" :dopełniacz "jeden" :celownik "jeden" :biernik "jeden"  :narzędnik "jeden" :miejscownik "jeden" :wołacz "jeden"}
              }
               
            1 {
               :lp {:mianownik "tysiąc" :dopełniacz "tysiąca" :celownik "tysiącowi" :biernik "tysiąc"  :narzędnik "tysiącem" :miejscownik "tysiącu" :wołacz "tysiąc"}
               :lm {:mianownik "tysiące" :dopełniacz "tysięcy" :celownik "tysiącom" :biernik "tysiące" :narzędnik "tysiącami" :miejscownik "tysiącach" :wołacz "tysiące"}
               }
           }
        ls ["milion" "miliard" "bilion" "biliard" "trylion" "tryliard" "kwadrylion" "kwadryliard" "kwintylion" "kwyntilard" "sekstylion" "sekstyliard" "septylion" "septyliard" "oktylion" "oktyliard" "nonilion" "noniliard" "decylion" "decyliard" "undecylion" "undecyliard" "duodecylion" "duodecyliard"]
        ;singular declension for ending -ion
        dec-lp-ion (fn [numeral] {:mianownik numeral :dopełniacz (str numeral "a") :celownik (str numeral "owi") :biernik numeral :narzędnik (str numeral "em") :miejscownik (str numeral "ie") :wołacz numeral})
        ;singular declension for ending -iard
        dec-lp-iard (fn [numeral] {:mianownik numeral :dopełniacz (str numeral "a") :celownik (str numeral "owi") :biernik numeral :narzędnik (str numeral "em") :miejscownik (str numeral "zie") :wołacz numeral})
        ;plural declension
        dec-lm (fn [numeral] {:mianownik (str numeral "y") :dopełniacz (str numeral "ów") :celownik (str numeral "om") :biernik (str numeral "y") :narzędnik (str numeral "ami") :miejscownik (str numeral "ach") :wołacz (str numeral "y")})]
        
    (loop [bs as xs ls i 2]
      (if (empty? xs)
        bs
        (if (odd? i)
          (recur (assoc bs i {:lp (dec-lp-iard (first xs)) :lm (dec-lm (first xs))}) (rest xs) (inc i))
          (recur (assoc bs i {:lp (dec-lp-ion (first xs)) :lm (dec-lm (first xs))}) (rest xs) (inc i)))))))

;create the map for the powers of 1000
(def largeNumbersMap (createLargeNumbersMap))
