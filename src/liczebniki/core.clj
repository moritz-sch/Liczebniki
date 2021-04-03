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


(ns liczebniki.core
  (:require [liczebniki.maps.createmapscardinal :refer [smallerThan20Map tensMap hundredsMap largeNumbersMap]]
            [liczebniki.maps.createmapscollective :refer [smallerThan20MapCollective tensMapCollective]]
            [liczebniki.maps.createmapsnoun :refer [nounMap]]
            [liczebniki.jeden :refer [printJedenCardinalColl]]
            [liczebniki.outputstyle :refer :all])
  (:gen-class))


;---------------------------------------
; some auxiliary functions
;---------------------------------------

(defn power
  "Calculates b to the n-th power"
  [b n]
  (reduce * (repeat n b)))

(defn numberOfDigits
  "Returns the number of digits of a natural number n"
  [n]
  (loop [i 10 j 1]
    (if (< n i)
      j
      (recur (* 10 i) (inc j)))))

(defn digitGroup
  "If we divide a decimal representation of a natural number n in groups of i digits, this function gives the p-th such group (where the 0-th one is the one corresponding to the last three digits)."
  [n p i]
  (let [q (* p i)
        t (rem n (power 10 (+ q i)))
        pq (power 10 q)]
    (/ (- t (rem n pq)) pq)))

(defn groupDigits
  "Divides the digits of a natural number n into groups of 3 and returns them in a map, with the last three digits corresponding to the key 0 and so on"
  [n]
  (let [nd (numberOfDigits n)
        ;m is the number of digit groups
        m (int (/ (dec nd) 3))
        ;ys contains vectors with the keys and the digit groups to be turned into a map
        ys (map (fn [i] (let [x (- m i)]
                         [x (digitGroup n x 3)])) (range (inc m)))]
    (apply (partial assoc {}) (flatten ys))))

;---------------------------------------
; functions for cardinal numbers
;---------------------------------------

(defn getNumberNamesCardinalTo999Cardinal
  "Returns the name(s) of a number from 2 to 999 in genus g, case c, output is a vector containing strings or maps of strings which may include nils"
  [n g c]
  (let [onesDigit         (rem n 10)
        onesAndTensDigits (rem n 100)
        tensDigit         (/ (- onesAndTensDigits onesDigit) 10)
        hundredsDigit     (/ (- (rem n 1000) onesAndTensDigits) 100)]
    ;names are taken from tables and put together
    (if (< onesAndTensDigits 20)
      [(get-in hundredsMap [hundredsDigit g c]) (get-in smallerThan20Map [onesAndTensDigits g c])]
      [(get-in hundredsMap [hundredsDigit g c]) (get-in tensMap [tensDigit g c]) (get-in smallerThan20Map [onesDigit g c])])))

(defn usedGenus
  "Auxiliary function for getNumberNamesCardinal: handles the cases in which parts of the numeral's genus does not match the noun's genus"
  [g numberOfGroups onesDigit tensDigit]
  (if (and (= g :f) (== onesDigit 2) (not= tensDigit 1)(not= 1 numberOfGroups))
    :nonmp
    (if (and (= g :mp) (not= 1 numberOfGroups))
      :nonmp
      g)))

;note that this function is never called with c = :wołacz as it is always identical with mianownik for numbers so wołacz is not contained in the map, the function getCleanNumberNames takes care of wołacz
(defn getNumberNamesCardinal
  "Returns a list of the valid names of the number n in the genus g and case c"
  [n g c]
  ;we group the digits in groups of three, find the words for each such group and put them together
  (loop [dms (groupDigits n)
         zs []]
    (if (empty? dms)
      zs
      (let [fdms (first dms)]
        ;the case that one of the groups is 0 or 1 must be handled individually
        (if (contains? #{0 1} (second fdms))
          (recur (rest dms) (conj zs (get [nil (get-in largeNumbersMap [(first fdms) :lp c])] (second fdms))))
          (let [onesDigit          (rem (second fdms) 10)
                onesAndTensDigits  (rem (second fdms) 100)
                tensDigit          (/ (- onesAndTensDigits onesDigit) 10)
                uGenus             (usedGenus g (count dms) onesDigit tensDigit)]
            ;handle the cases in which the case of the number and the case of the power of 1000 do not match
            (if (or (= c :mianownik) (= c :biernik))
              (if (or (contains? #{1 5 6 7 8 9 0} onesDigit) (contains? #{12 13 14} onesAndTensDigits))
                (recur (rest dms) (conj (into zs (getNumberNamesCardinalTo999Cardinal (second fdms) uGenus c)) (get-in largeNumbersMap [(first fdms) :lm :dopełniacz])))
                (if (and (= g :mp) (== 1 (count dms)))
                  (recur (rest dms) (conj (into zs (getNumberNamesCardinalTo999Cardinal (second fdms) uGenus c)) (get-in largeNumbersMap [(first fdms) :lm :dopełniacz])))
                  (recur (rest dms) (conj (into zs (getNumberNamesCardinalTo999Cardinal (second fdms) uGenus c)) (get-in largeNumbersMap [(first fdms) :lm c])))))
              (recur (rest dms) (conj (into zs (getNumberNamesCardinalTo999Cardinal (second fdms) uGenus c)) (get-in largeNumbersMap [(first fdms) :lm c]))))))))))

(defn extractAllValidNamesAux
  "Auxiliary function for extractAllValidNames, for the special case n mod 10=2, feminine, narzędnik, where more than two valid words may occur"
  [xs a b]
  (loop [ys xs zs [] i (count xs)]
    (if (== i 0)
      zs
      (if (== i 1)
        (recur (rest ys) (conj zs (get (first ys) b)) (dec i))
        (if (map? (first ys))
          (recur (rest ys) (conj zs (get (first ys) a)) (dec i))
          (recur (rest ys) (conj zs (first ys)) (dec i)))))))

(defn extractAllValidNames
  "Let n g c be as in getNumberNamesCardinal and namesNoNil the output of said function with all nils removed, then this function returns a list of all valid names for n in genus g and case c with each word given as a list of strings"
  [namesNoNil n g c]
  (if (and (= g :f) (= c :narzędnik) (== 2 (rem n 10)))
    (map (fn [[a b]] (extractAllValidNamesAux namesNoNil a b)) [[1 1] [1 2] [2 1] [2 2]])
    (map (fn [i] (map (fn [x] (if (map? x)
                   (get x i)
                   x
                 )) namesNoNil)) [1 2])))

(defn listToString
  "Takes a list of strings and returns their concatenation with whitespaces between each one"
  [xs]
  (apply str (interpose " " xs)))


(defn getCleanNumberNames
  "Returns a list of all valid names of the number n in genus g and case c as a list of strings"
  [n g c]
  ;since :wołacz is not in the map as it is identical to mianownik, it is handled here
  (if (= c :wołacz)
    (let [names        (getNumberNamesCardinal n g :mianownik)
          namesNoNil   (filter #(not (nil? %)) names)]
      ;doesn't matter if c is :mianownik or :wołacz in this line
      (map listToString (distinct (extractAllValidNames namesNoNil n g :mianownik))))
    (let [names        (getNumberNamesCardinal n g c)
          namesNoNil   (filter #(not (nil? %)) names)]
      (map listToString (distinct (extractAllValidNames namesNoNil n g c))))))

;auxiliary function for getCleanNamesAndRequiredCases
(defn endsInPowerOf1000?
  "Determines if the last component of the Polish name is a power of 1000"
  [n]
  (let [ds (reverse (map second (groupDigits n)))]
    (if (not= (first ds) 0)
      false
      (let [es (drop-while #(== % 0) ds)]
        (== (first es) 1)))))

(defn getCleanNamesAndRequiredCases
  "Returns a list of pairs where the first entries are the valid names of the number n in genus g and case c and the second entries are the cases they require"
  [n g c]
  (if (and (= g :mp) (= c :mianownik) (contains? #{2 3 4} n))
    ;special cases
    ({
      2 '(["dwóch" :dopełniacz] ["dwaj" :mianownik])
      3 '(["trzech" :dopełniacz] ["trzej" :mianownik])
      4 '(["czterech" :dopełniacz] ["czterej" :mianownik])
     } n)
    (let [cleanNames        (getCleanNumberNames n g c)
          onesDigit         (rem n 10)
          onesAndTensDigits (rem n 100)]
      (if (endsInPowerOf1000? n)
        ;special case in which all cases require dopełniacz
        (map (fn [x] [x :dopełniacz]) cleanNames)
        ;remaining cases, in mp the case of the numeral and the noun don't always match
        (if (or (= c :mianownik) (= c :biernik) (= c :wołacz))
          (if (or (= g :mp) (contains? #{0 1 5 6 7 8 9} onesDigit) (contains? #{12 13 14}onesAndTensDigits))
            (map (fn [x] [x :dopełniacz]) cleanNames)
            (map (fn [x] [x c]) cleanNames))
          (map (fn [x] [x c]) cleanNames))))))

;---------------------------------------
; functions for collective numbers
;---------------------------------------

(defn getNumberNamesTo999Coll
  "Returns the name of a collective number from 2 to 999 in case c, output is a vector of strings which may include nils if it is unique, otherwise a vector of two such vectors"
  [n c]
  (let [onesDigit         (rem n 10)
        onesAndTensDigits (rem n 100)
        tensDigit         (/ (- onesAndTensDigits onesDigit) 10)
        hundredsDigit     (/ (- (rem n 1000) onesAndTensDigits) 100)
        cc                (if (= c :wołacz)
                            :mianownik
                            c)]
    (if (== n 0)
      [""]
      (if (== onesAndTensDigits 0)
        [(get-in hundredsMap [hundredsDigit :nonmp cc])]
        (if (< onesAndTensDigits 20)
          [(get-in hundredsMap [hundredsDigit :nonmp cc]) (get-in smallerThan20MapCollective [onesAndTensDigits c])]
          (if (== onesDigit 0)
            ;in this case the tens digit must be a collective numeral
            [(get-in hundredsMap [hundredsDigit :nonmp cc]) (get-in tensMapCollective [tensDigit c])]
            ;the tens digit may be a collective or cardinal numeral
            [[(get-in hundredsMap [hundredsDigit :nonmp cc]) (get-in tensMapCollective [tensDigit c]) (get-in smallerThan20MapCollective [onesDigit c])]
             [(get-in hundredsMap [hundredsDigit :nonmp cc]) (get-in tensMap [tensDigit :nonmp cc]) (get-in smallerThan20MapCollective [onesDigit c])]]))))))

(defn extractAllValidNamesColl
  "If name is a seq of strings or maps of strings depicting the different collective numerals with the ambiguity stemming from the tens digit which is written as a cardinal number, this function returns a seq of all names as a seq of seqs of strings"
  [names]
  (distinct (map (fn [i] (map (fn [x] (if (map? x)
                 (get x i)
                 x
               )) names)) [1 2])))

(defn getNumberNamesColl
  "Returns the names of a collective number n in case c, output is a seq of one to four elements which are seqs of strings which may include nils, if c is :dopełniacz or :miejscownik output may not be duplicate free"
  [n c]
  (let [m  (rem n 1000)
        t  (- n m)
        on  (getNumberNamesTo999Coll n c)
        ;ons is a list of the names of the last three digit group of n, given as a list of strings
        ons (extractAllValidNamesColl (second on))]
    (if (== t 0)
      ;n is between 1 and 999
      (if (vector? (first on))
        (concat (list (first on)) ons)
        (list on))
      ;n is >= 1000
      (let [tn (map first (getCleanNamesAndRequiredCases t :nonmp c))]
        (if (vector? (first on))
          ;the case that there is more than one collective numeral for the last three digit group of n
          (if (some map? (second on))
            ;more than one version of the larger part of the numeral?
            (if (> (count tn) 1)
              ;first map has the names where the ones and tens digits are collective, the second map where the ones digit is collective, the tens digit cardinal, we only put the matching entries together in the second map
              (concat (map (fn [i] (concat [(nth tn i)] (first on))) [0 1])
                      (map (fn [i] (concat [(nth tn i)] (nth ons i))) [0 1]))
              ;same as above
              (concat (concat tn (first on))
                      (map (fn [i] (concat tn (nth ons i))) [0 1])))
            ;ambiguity in on comes only from the tens digit and whether it is collective or cardinal, in this case all combinations are valid
            (for [a tn
                  b on]
              (concat [a] b)))
          ;the case that the last group has a unique collective numeral
          (map (fn [x] (concat [x] on)) tn))))))

(defn getCleanNamesAndRequiredCaseColl
  "Returns the name of a collective number n in case c plus the case it requires, output is a vector with those two values as entries"
  [n c]
  ;distinct is needed because the above function may give duplicate entries because of the ambiguities of cardinal numbers in dopełniacz and miejscownik
  (let [names        (distinct (getNumberNamesColl n c))
        namesNoNil   (map (fn [xs] (filter #(and (not (= "" %)) (not (nil? %))) xs)) names)
        cleanNames   (map listToString namesNoNil)]
    (if (or (= c :mianownik) (= c :biernik) (= c :narzędnik) (= c :wołacz) (endsInPowerOf1000? n))
      (map (fn [x] [x :dopełniacz]) cleanNames)
      (map (fn [x] [x c]) cleanNames))))

;---------------------------------------
; function for numbers as nouns
;---------------------------------------

(defn getNameNoun
  "Returns the name of the number n as a noun in case c and numerus nm if it exists, otherwise nil"
  [n nm c]
  (let [x (nounMap n)]
    (if x
      (get-in x [nm c])
      nil)))

;---------------------------------------
; print functions for cardinal numbers
;---------------------------------------

(defn printDeclensionTableLine
  "Prints one line of the declension table of a cardinal or collective number, input is the output of getCleanNamesAndRequiredCases"
  [ls]
  (loop [xs ls]
    (let [rs (rest xs) fxs (first xs)]
      (if fxs
        (if (empty? rs)
          (do
            (print (style (first fxs) :blue))
            (print (style " (+" :teal))
            (print (style (caseNameMap (second fxs)) :teal))
            (print (style ")" :teal)))
          (do
            (print (style (first fxs) :blue))
            (print (style " (+" :teal))
            (print (style (caseNameMap (second fxs)) :teal))
            (print (style ")" :teal))
            (print (style " / " :reset))
            (recur rs)))))))

(defn printDeclensionCardinal
  "Prints the declension table of the cardinal number n in genus g, singular"
  [n g]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]]
    (loop [cs cases]
      (if (empty? cs)
        nil
        (do
          (print (style (caseNameMapFormatted (first cs)) :purple))
          (let [xs (getCleanNamesAndRequiredCases n g (first cs))]
            (printDeclensionTableLine xs)
            (newline))
          (recur (rest cs)))))))

(defn printDeclensionCardinalPlural
 "Prints the declension table of the cardinal number n, plural (which only exists for powers of 1000, where genus is irrelevant)"
 [n]
 (let [i (/ (dec (numberOfDigits n)) 3)
       cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]]
   (loop [cs cases]
     (if (empty? cs)
       nil
       (do
         (print (style (caseNameMapFormatted (first cs)) :purple))
         (let [x  (get-in largeNumbersMap [i :lm (first cs)])
               xs (list [x :dopełniacz] )]
           (printDeclensionTableLine xs)
           (newline))
         (recur (rest cs)))))))

(defn printAllDeclensionsCardinal
  "Prints all declension tables of the cardinal number n"
  [n]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]
        xsmp    (map (fn [c] (getCleanNamesAndRequiredCases n :mp c) ) cases)
        xsnonmp (map (fn [c] (getCleanNamesAndRequiredCases n :nonmp c) ) cases)
        xsf     (map (fn [c] (getCleanNamesAndRequiredCases n :f c) ) cases)
       ]
    ;this is only true for the powers of 1000 and their composites, only for powers of 1000 do we have a plural
    (if (= xsmp xsnonmp)
      ;for the powers of 1000 this is the same as the cardinal numbers (Clojure can't handle larger numbers than the ones in the set)
      (if (contains? #{(power 10 3) (power 10 6) (power 10 9) (power 10 12) (power 10 15) (power 10 18)} n)
        (do
          (printHeadline "All Cases, singular (liczba pojedyńcza)")
          (printDeclensionCardinal n :mp)
          (newline)
          (newline)
          (printHeadline "All Cases, plural (liczba mnoga)")
          (printDeclensionCardinalPlural n))
        ;composites
        (do
          (printHeadline "All Cases, singular (liczba pojedyńcza)")
          (printDeclensionCardinal n :mp)))
      (do
        (printHeadline "Masculine Personal")
        (printDeclensionCardinal n :mp)
        (newline)
        (newline)
        (if (= xsnonmp xsf)
          (do
            (printHeadline "Feminine, Neutral, Masculine Nonpersonal")
            (printDeclensionCardinal n :nonmp))
          (do
            (printHeadline "Feminine")
            (printDeclensionCardinal n :f)
            (newline)
            (newline)
            (printHeadline "Neutral, Masculine Nonpersonal")
            (printDeclensionCardinal n :nonmp)))))))

;---------------------------------------
; print functions for collective numbers
;---------------------------------------

(defn printDeclensionColl
  "Prints the declension table of the collective number n"
  [n]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]]
    (loop [cs cases]
      (when (not (empty? cs))
        (do
          (print (style (caseNameMapFormatted (first cs)) :purple))
          (printDeclensionTableLine (getCleanNamesAndRequiredCaseColl n (first cs)))
          (newline)
          (recur (rest cs)))))))

;---------------------------------------
; print functions for numbers as nouns
;---------------------------------------

(defn printDeclensionTableLineNoun
  "Prints a line in the declension table of a number as a noun, input is the output of getNameNoun"
  [x]
  (print (style x :blue)))

(defn printDeclensionNoun
  "Prints singular or plural declension tables for the natural number n as a noun, nm determines which one, the values are :lp for singular and :lm for plural"
  [n nm]
  (let [cases [:mianownik :dopełniacz :celownik :biernik :narzędnik :miejscownik :wołacz]]
    (loop [cs cases]
      (when (not (empty? cs))
        (do
          (print (style (caseNameMapFormatted (first cs)) :purple))
          (printDeclensionTableLineNoun (getNameNoun n nm (first cs)))
          (newline)
          (recur (rest cs)))))))

(defn printAllDeclensionsNoun
  "Prints singular and plural declension tables for the natural number n as a noun"
  [n]
  (printHeadline "Singular (liczba pojedyńcza)")
  (printDeclensionNoun n :lp)
  (newline)
  (newline)
  (printHeadline "Plural (liczba mnoga)")
  (printDeclensionNoun n :lm))

;---------------------------------------
; putting the output functions together
;---------------------------------------

(defn printAll
  "Prints all declension tables for a given natural number n"
  [n]
  (let [polishName (first (getCleanNumberNames n :nonmp :mianownik))
        nounName   (getNameNoun n :lp :mianownik)]
    (newline)
    (print (style (str "          Showing declensions for " n " (" polishName ")") :purple))
    (newline)
    (newline)
    (if (== n 1)
      ;for 1 this part is hard-coded as it is highly irregular
      (printJedenCardinalColl)
      (do
        (printBigHeadline "Cardinal numbers (liczebniki główne)")
        (printAllDeclensionsCardinal n)
        (newline)
        (newline)
        (printBigHeadline "Collective numbers (liczebniki zbiorowe)")
        (printDeclensionColl n)
        (newline)
        (newline)))
    (printBigHeadline "As a noun (rzeczowniki odliczebnikowe)")
    ;for the powers of 1000 this is the same as the cardinal numbers (Clojure can't handle larger numbers than the ones in the set)
    (if (contains? #{(power 10 3) (power 10 6) (power 10 9) (power 10 12) (power 10 15) (power 10 18)} n)
      (println "See cardinal numbers.")
      (if nounName
        (do
          (printAllDeclensionsNoun n)
          (newline))
        (do
          (print (style "All Cases     " :purple))
          (print (style (first (getCleanNumberNames n :nonmp :mianownik)) :blue)))))
    (newline)
    (newline)))

;---------------------------------------
; input
;---------------------------------------

(defn checkValidity
  "Checks validity of user input and returns cleaned input if it was valid, false if it was invalid"
  [string]
  (if (or (= string "exit") (= string "quit"))
    :exit
    (try
      (Long. string)
      (catch Exception e
        (println (style "Input cannot be converted to integer, please enter a natural number or \"quit\" or \"exit\" to quit." :bold))
        false))))

(defn getInput
  "Waits for user input and prints the corresponding declension tables if it is an integer in the permissible range, quits the program if it is \"quit\" or \"exit\" or asks for new input otherwise"
  []
  (let [input (read-line)]
    (let [v (checkValidity input)]
      (if v
        (if (= v :exit)
          (System/exit 0)
          (if (<= v 0)
            (do
              (println (style "Only positive numbers are allowed as input." :bold))
              (println (style "Please enter a natural number or \"quit\" or \"exit\" to quit." :bold))
              (recur))
            (do
              (printAll v)
              (newline)
              (println (style "Please enter a natural number or \"quit\" or \"exit\" to quit." :bold))
              (recur))))
        (recur)))))

(defn begin
  "Starting the program"
  []
  (newline)
  (newline)
  (println (style "Liczebniki" :bold))
  (println "A program for showing declension tables of Polish numerals")
  (println "Enter a natural number or \"quit\" or \"exit\" to quit.")
  (println "\n")
  (getInput))

;---------------------------------------
; main
;---------------------------------------

(defn -main
  [& args]
  (begin))
