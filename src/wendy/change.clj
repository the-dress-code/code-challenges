(ns wendy.change
  (:gen-class))

(defn possible-solution
  [target coin-set]
  (reduce (fn [acc nxt]
            (let [any-count (rand-int (inc (quot target nxt)))]
              (if (pos? any-count)
                (assoc acc nxt any-count)
               acc))) 
        {} 
        coin-set))


(defn valid?
  [target solution]
  (= target (apply + (map (fn [[coin count]] (* coin count)) solution))))


(defn make-change
  [target coin-set]
  (loop [solution (possible-solution target coin-set)
         tries 0]
    (if (valid? target solution)
      (do (prn tries)  
          solution)
      (when (< tries 10000)
        (recur (possible-solution target coin-set) (inc tries))))))

(comment 
  (println (make-change 6 [1 5 10 25]))
  (println (make-change 6 [3 4]))
  (println (make-change 6 [1 3 4]))
  (println (make-change 6 [5 7]))
  (println (make-change 16 [5 7 9]))
)

; nov 6 2023, 1:00 AM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The journey begins.

; whats the first easiest thing you can do? 

; get rid of coins greater than x

(def x 6)

(def coinset [1 5 10 25])

(defn equal-to-or-less-than?
    [coin]
    (when (<= coin x)
      coin))

(defn goodbye-large-coins [x coinset]

  (let [initial []
        coll coinset]
    
    (loop [result initial
           remaining coll]

      (if (empty? remaining)

        result

        (recur 

         #_ (let [first-dude (first remaining)
                  _ (prn (str "first-dude: " first-dude))
                  keep-it (equal-to-or-less-than? first-dude)
                  _ (prn (str "keep-it: " keep-it))
                  new-coll (conj result keep-it)
                  _ (prn (str "new-coll: " new-coll))]

              new-coll)

         (let [first-dude (first remaining)
               keep-it (equal-to-or-less-than? first-dude)
               new-coll (filter some? (conj result keep-it))]
           new-coll)
         (rest remaining))))))

(goodbye-large-coins 6 [1 5 10 25]) ;; => (5 1)

; Oh yeesh, there is a simpler way!

(filter #(< % 6) [1 5 10 25])
;; => (1 5)

(defn keep-coins-equal-or-less
  [x coll]
  (filter #(< % x) coll))

(keep-coins-equal-or-less 6 [1 5 10 25])
;; => (1 5)

; whats the next smallest thing i can do?

; how many times does the largest coin go into x?

; x = 70
; coinset = [1 5 10 25]

(quot 70 25)
;; => 2

; what is the remainder if we divide x by the largest item in the coinset?

(rem 70 25) ;; => 20

; rem and quot build result while recuring

; x = 17
; coinset = [4 9 14 15 16 25]

;; what do i need to do to find a solution?

; 1. reverse the vector order

; (reverse coinset)

; 2. only keep coins that are less than or equal to x. (optional? could deal with this during recursion)

(defn coins-equal-or-less [x coinset]
  (filter #(< % x) coinset))

(coins-equal-or-less 70 [1 5 10 25 100])
;; => (1 5 10 25)

; 3. process the vector & build a new coll, which is a map : key = coin, val = result of (quot coin x)

; what needs to happen

(let [coin (first coinset)  ; get the first coin
      quotient (quot x coin) ; # of times coin goes into x. put result as map's v in new coll, coin is map's k.
      remainder (rem x coin)]) ; if result is 0, return new-coll. else bind result to x on loop if result is 0, stop and return new map.

; when do you stop and result the new-coll?

(if (zero? remainder)
  result)

(let [x 70
      coin 25]
  {coin (quot x coin)}) ;; => {25 2}

x = 17
coinset = [4 9 14 15 16 25]
---------------------------------------

(first (sort > [10 4 33 1])) ;; => (33 10 4 1)

(first (33 10 4))
;; => Execution error (ClassCastException) at metabase/eval7575 (REPL:184).
;;    class java.lang.Long cannot be cast to class clojure.lang.IFn (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IFn is in unnamed module of loader 'app')

(defn find-solution [x coinset]

  (loop [__ __
         __ __]

    (let [coin (first coinset)
          quotient (quot remaining-target coin)
          remainder (rem remaining-target coin)]

      remainder)

    (if (= (zero? remainder) (zero? remaining-target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;  recursion side-quest  ;;;;;;;;;;;;;;;;;;;

; what is the smallest example of recursion you can do

; 1. take collection.
; 2. do stuff to items in the collection.
; 3. repeat.  ;; use loop
; 4. til collection is processed. ;; use something that returns true or false to make it stop

;;;;;;; INCREMENT EACH ITEM IN COLL

(defn inc-them-all [x]

(loop [remaining x
       result []]

    (if (empty? remaining)

      result
      
      (let [first-dude (first remaining)
            bigger-dude (inc first-dude)
            incd-coll (conj result bigger-dude)
            small-coll (rest remaining)]

        (recur small-coll incd-coll)))))

(inc-them-all [1 2 3 4 5])
;; => [2 3 4 5 6]

;; SUCCESS!

;;;;;;;;  RETURN COLL OF ONLY ODD NUMBERS

(defn only-odds? [x]

  (loop
      [remaining x
       result []]

    (if (empty? remaining)
      
      result
      
      (let [first-item (first remaining)
            odd-dude (when (odd? first-item)
                       first-item)
            odd-coll (conj result odd-dude)
            small-coll (rest remaining)]

        (recur small-coll odd-coll)))))

(only-odds? [1 2 3 4 5 6 7 8]) 

;; => [1 nil 3 nil 5 nil 7 nil]

;; FAIL

;; Bah, frickin nils...frickin NILS!

; why are my nils there?

; the implied else is returning nil
; nil is being conj'd onto result


;;;;;;;;;  RETURN COLL OF ONLY ODD NUMBERS, DONT LET THE NILS IN, DO IT RIGHT, NO NIL FILTERING

(defn only-odds [x]

  (let [coll x
        initial []]

    (loop [result initial
           remaining coll]

      (if (empty? remaining)

        result

        (recur (let [first-item (first remaining)
                     new-coll (if (odd? first-item)
                                (conj result first-item)
                                result)]
                 new-coll)
               
               (rest remaining)))))) 

(only-odds [1 2 3 4 5 6 7 8])

;; => [1 3 5 7]

;; SUCCESS!

;;;;;;;;;  INCREMENT ALL THE EVEN NUMBERS IN GIVEN NESTED COLL

(def simple-vector [2 3 5 [90 13 15 7 [22 21 3389 78 90]] [20 21 23] 9 10 45 56])

(defn inc-the-evens-dep [x]

  (let [initial []
        coll (flatten x)]

    (loop [result initial
           remaining coll]

      (if (empty? remaining)
        
        result

        (recur (let [first-item (first remaining)
                     new-coll (if (even? first-item)
                                (conj result (inc first-item))
                                result)]
                 new-coll)
               
               (rest remaining))))))

(inc-the-evens-dep simple-vector)

;; => [3 91 23 79 91 21 11 57]

;; FAIL

; Oops! I changed the shape of the data and threw out the odds.
; Re-read the problem!
; dont change shape of the data - no flatten
; dont omit the odd numbers
; inc the even numbers


;;;;;;;;;;;;  INC ALL EVEN NUMBERS IN NON-NESTED COLL

(def simplest-vector [2 3 4 5 6 7]) ; no nesting

(let [initial []
      coll simplest-vector]

  (loop [result initial
         remaining coll]

    (if (empty? remaining) ; remaining is the name we bound to the coll we supplied
      
      result ; what we are building

      (recur ; recur needs as many args as supplied in loop 

       (let [first-item (first remaining) ; first item of the provided coll. bound to "first-item"
             new-coll (if (even? first-item) ; if first-item is even, then
                        (conj result (inc first-item)) ; inc first-item  + conj to result
                        (conj result first-item))] ; conj to result
         new-coll) ; current new-coll
       
       (rest remaining))))) ; current remaining, without the first item

;; => [3 3 5 5 7 7]

;; SUCCESS!

;  no nils, baby!


;;;;;;;;;;   RECURSION ON SIMPLER NESTED COLL

; mentally / physically walk through how you'd deal with items in the coll as you encountered them - this helped!

; seems like i just need to run another recur show when i hit a coll, right?

(def simpler-vector [1 202 [53 466] 7])

(defn nest-inspector-dep [x]

  (let [initial []
        coll x]
    (loop
        [result initial 
         remaining coll]

      (if (empty? remaining) ; checks to see if remaining (of the orig. coll) is empty

        result ; returns the result we're building

        (recur ; the following number of args must match that in loop
         
         (let [first-item (first remaining) ; binds the symbol "first-item" to the first item of remaining (our provided collection.)
              _ (prn (str "first-item: " first-item))
               new-coll (if (coll? first-item) ; if first-item is a coll

                          (conj result (first first-item)) ; then, get the first item of first-item and conj to result

                          (conj result first-item))] ; otherwise, conj first-item to result
                                        ; either way, whatever comes out is bound to "new-coll"
           (prn (str "new-coll: " new-coll))
           new-coll) ; current new-coll
         (rest remaining))))))  ; current remaining, without the first item

(nest-inspector-dep simpler-vector) 

;; => [1 202 53 7]

;; FAIL

; repl output:

; "first-item: 1"
; "new-coll: [1]"
; "first-item: 202"
; "new-coll: [1 202]"
; "first-item: [53 466]"
; "new-coll: [1 202 53]"
; "first-item: 7"
; "new-coll: [1 202 53 7]"

; whoops, we lost 2 things:

; 1. last item in inner vector of simpler-vector.

; 2. lost inner

;; Q: what should  we do when you hit a collection, within our coll?

;; A: call the fn on itself! - how will we know when to move on? inner coll is empty, just like with outer coll.


;;;;;;;;;;; USE RECURSION ON SIMPLER NESTED COLLECTION (NO INC)

(prn "------START OVER------")

(def simpler-vector [1 202 [53 466] 7])

(defn nest-inspector [x]

  (let [initial []
        coll x]

    (loop
        [result initial 
         remaining coll]

      (if (empty? remaining) ; checks to see if remaining (of the orig. coll) is empty

        result ; returns the result we're building

        (recur ; the following # of args must match loop ; start over at loop ; recur says lets do it again!
         
         (let [first-item (first remaining)
               _ (prn (str "first-item: " first-item))
               new-coll (if (coll? first-item) ; if first-item is a coll

                          (conj result (nest-inspector first-item)) ; call nest-inspector on the coll! conj to result

                          (conj result first-item))] ; else  conj first-item to result
                                                     ; either way, whatever comes out is bound to "new-coll"
           (prn (str "new-coll: " new-coll))
           new-coll) ; current new-coll
         (rest remaining))))))  ; current remaining, without the first item

(nest-inspector simpler-vector) 

;; => [1 202 [53 466] 7]

;; SUCCESS!

; REPL OUTPUT

; "first-item: 1"
; "new-coll: [1]"
; "first-item: 202"
; "new-coll: [1 202]"
; "first-item: [53 466]"
; "first-item: 53"
; "new-coll: [53]"
; "first-item: 466"
; "new-coll: [53 466]"
; "new-coll: [1 202 [53 466]]"
; "first-item: 7"
; "new-coll: [1 202 [53 466] 7]"

(def simple-vector [2 3 5 [90 13 15 7 [22 21 3389 78 90]] [20 21 23] 9 10 45 56])

(nest-inspector simple-vector)

;; => [2 3 5 [90 13 15 7 [22 21 3389 78 90]] [20 21 23] 9 10 45 56]

;; SUCCESS! Saturday

; SUNDAY - now go back and try again - write it fresh

;;;;;;;;;  INC ALL THE EVEN NUMBERS IN GIVEN NESTED COLL

; x can u process the nest?

(defn complicated-nest-process [x]

  (let [initial []
        coll x]

    (loop [result initial
           remaining coll]

      (if (empty? remaining)

        result

        (recur

         (let [first-item (first remaining)
               new-coll (if (coll? first-item)
                          (conj result (complicated-nest-process first-item))
                          (conj result first-item))]
           new-coll) ; the whole point of this let is to get a new collection. the return value of this let is new-coll.

         (rest remaining))))))

(complicated-nest-process [1 2 3 [4 5] 6]) ;; => [1 2 3 [4 5] 6]

; x simplfy processing a nested collection (move work out of recur)

(defn simple-nest-process [x]

  (loop [result [] 
         remaining x]

    (if (empty? remaining)

      result

      (let [first-item (first remaining)

            new-coll (if (coll? first-item)
                       (conj result (simple-nest-process first-item))
                       (conj result first-item))]

        (recur new-coll (rest remaining))))))

(simple-nest-process [1 2 [4] 4])
;; => [1 2 [4] 4]

; x can you write a fn to inc-the-evens?

(defn complicated-inc-the-evens [x]

  (let [initial []
        coll x]

    (loop [result initial
           remaining coll]

      (if (empty? remaining)
        
        result

        (recur (let [first-item (first remaining)
                     new-coll (if (even? first-item)
                                (conj result (inc first-item))
                                result)]
                 new-coll)
               
               (rest remaining))))))

(complicated-inc-the-evens [1 2 3 4])
;; => [3 5]

;; FAIL - you lost the odd numbers.

;; update: you don't need this fn to solve for INC THE EVEN NUMBERS IN A NESTED COLLECTION

(defn simple-inc-the-evens [x]

  (loop [result [] ; binding
         remaining x] ; binding

    (if (empty? remaining) ; 

      result

      (let [first-item (first remaining)
            new-coll (if (even? first-item)
                       (conj result (inc first-item))
                       (conj result first-item))]

        (recur new-coll (rest remaining))))))

(simple-inc-the-evens [1 2 3 4 5 6])
;; => [1 3 3 5 5 7]

; x can you work simple-inc-the-evens into simple-nest-process?

; x  insert the work to check if first-item is even. inc if even, otherwise, give me first-item.

(defn simple-nest-process-and-inc-dec [x]

  (loop [result [] ; binding
         remaining x] ; binding

    (if (empty? remaining) ; 

      result

      (let [first-item (first remaining)
            _ (prn (str "first-item: " first-item)) 
            new-coll (if (coll? first-item)
                     (conj result (simple-nest-process-and-inc-dec first-item))
                     (conj result (simple-inc-the-evens first-item)))
            _ (prn (str "new-coll: " new-coll))] 

        (recur new-coll (rest remaining))))))

(simple-nest-process-and-inc [1 2 3 [4 5] 6])
;; => Execution error (IllegalArgumentException) at wendy.change/simple-inc-the-evens (REPL:508).
;;    Don't know how to create ISeq from: java.lang.Long

(prn "------START OVER------")

; wait, why is simple-inc-the-evens-dec recursive? 

; inc-the-evens just needs to be the machine that incs just one number if its even. so write that.

;;;;;;;;;  INC ALL THE EVEN NUMBERS IN GIVEN NESTED COLL

(def simple-vector [2 3 5 [90 13 15 7 [22 21 3389 78 90]] [20 21 23] 9 10 45 56])

(defn inc-if-even [x]
  (if (even? x)
    (inc x)
    x))

(defn inc-evens-in-nest [x]

  (loop [result []
         remaining x]

    (if (empty? remaining)

      result

      (let [first-item (first remaining)

            new-coll (if (coll? first-item)
                       (conj result (inc-evens-in-nest first-item))
                       (conj result (inc-if-even first-item)))] 

        (recur new-coll (rest remaining))))))

(inc-evens-in-nest [1 2 3 [4 5] 6])
;; => [1 3 3 [5 5] 7]

(def simple-vector [2 3 5 [90 13 15 7 [22 21 3389 78 90]] [20 21 23] 9 10 45 56])

(inc-evens-in-nest simple-vector)
;; => [3 3 5 [91 13 15 7 [23 21 3389 79 91]] [21 21 23] 9 11 45 57]

;; SUCCESS! THIS ACTUALLY SOLVES THE PROBLEM. YOU ARE DONE WITH THIS EXERCISE :) :)


(comment 

;; Analyze line by line.

; - assume its all wrong and you have to prove to yourself that it is right.

; - every time i do something, say what i am doing with the result of that thing.

; - the answer is never to the right of what you're asking about.

; - for example, what are you doing with the result of let?  im returning it as the val of my fn

; - imagine each fn is replaced with its value. what are you doing with that value?

; - everything is a black box - what are you doing with the result of the black box?

; - be vigilant of what im actually doing VS what i want to be doing = this is where bugs live

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; BACK TO THE ACTUAL PROBLEM ;;;;;;;;;;;;;;;;;

;; What do you have? 

; - target x

; - ascending coinset

;; What is the goal?

; - Return a map such that each key is the coin, and each value is the number of times you need that coin. 

; x If x = 17 and coinset = [4 9 14 25], possible solution is {:4 2, :9 1, :14 0, :25 0}

;; What information do I need to accomplish goal / what do I want?

; x My coinset is sorted. (description indicates it's sorted)

; x How is my coinset sorted? I want it descending so I fetch largest coins first. (reverse coinset)

; x How many times does the first coin of the sorted coinset go into the target?  (quot target coin); {coin (quot target coin)}

; x I need to know the remainder of original target and first coin of sorted coinset. result of (rem target coin) will be our new target for next coin.

; - How many times does the first coin of the sorted coinset go into the target? 

; - Does the target currently equal 0?

; - Is the sorted coinset empty?

;; What should we do when the following cases are true?

; - 1. Target = 0, Remaining = empty ; I'm done - Return result

; - 2. Target = 0, Remaining = not empty ; I'm done - Return result

; - 3. Target = not 0, Remaining = not empty ; Keep going - Return target - keep processing like "normal"

; - 4. Target = not 0, Remaining = empty ; Keep going but with extra work :

; -------------------------------------  In the result so far, dec the val of largest key with a non-0 val. This is now your new result.

; -------------------------------------  Start over with new result as described above, remaining as coll not including other coins in result, target as original

(def x 17)
(def coinset [4 9 14 25])

; What is ONE SMALL THING you can do to accomplish one goal from above?

; x How is my coinset sorted? I want it descending so I fetch largest coins first.

(defn reverse-it [coinset]
  (reverse coinset))

(reverse-it coinset) ;; => (25 14 9 4)

; x How many times does the first coin of the sorted coinset go into the target?  

(quot x coin)
(quot 17 9) ; = 1

(defn coin-count [x coinset]
  (let [coin (first coinset)
        count (quot x coin)]
    {coin count}))

(coin-count 17 [25 14 9 4])
;; => {25 0}

; x I need to know the remainder of original target and first coin of sorted coinset. result of (rem target coin) will be our new target for next coin.

(let [x 17
      coin 14
      new-target (rem x coin)]
  new-target)
;; => 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       
(loop ; anything u need to keep track of, you put in your loop

    [result {}
     target x
     remaining coinset])

 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [initial {}
      target x
      coll coinset]

  (loop [result initial
         remaining coll]

                                        ; START HERE

                                        ; - Create tests

    (if (empty? remaining)
      
      (if (zero? target)

        result  ; do this if target = 0, remaining = empty
        
        (recur ; do this if target = not 0, remaining = empty

         (let [ (result)]

           result) ; what do you want to do to result? find the largest key, dec its val. update the key-val to this new val.
                                        ; what could result look like at this point? {24 1 10 2}
                                        ; step 1 find largest key or first key
         

         remaining)))))


;;;;;;;;;;;;;;;;;;;;;;

; how to get dec'd val from largest key

(-> {25 2 10 2}
    first
    val
    dec)
;; => 1

(dec (val (first {25 2 10 2})))
;; => 1

; update the val to this new val

(update m k f x y)

(def p {:name "James" :age 26})
;;=> #'user/p

(update p :age inc)
;;=> {:name "James", :age 27}

(def result {25 2 10 2})

(update result 25 dec)
;; => {25 1, 10 2}

; cool! I don't need the threading.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What expressions would you use in an if statement? 

; if (zero? target) and (empty? remaining) -> result

; if (zero? target) and (seq remaining) -> result

; if (not= 0 target) and (seq remaining) -> Keep going - Return target - keep processing like "normal"

; if (not= 0 target) and (empty? remaining) -> do #4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What would it look like as a cond?

(cond
  (zero? target) result ; if target = 0, result. if target not 0, go to next ------->
  (empty? remaining) ; if true, target = not 0 and remaining empty, do work listed below in 4. if target = not 0 and remaining not empty , go to next ------->
  :else ; if target = not 0 and remaining not empty, then keep going, return / use target, and keep processing like "normal".
 ; (seq remaining) ; turns out i dont need this
 ; (not= 0 target) ; turns out i dont need this
  ) 

; 4.  Keep going but with extra work :

; In the result so far, dec the val of largest key with a non-0 val. This is now your new result. 

(let [result {25 2 10 2}]
  (update result 25 dec))
;; => {25 1, 10 2}

; Start over with new result as described above. remaining should be the coll not including other coins in result, target as original

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#_(defn change1 [x coinset]

    (loop [result {}
           remaining coinset
           target x]

      (let [coin (first (reverse remaining))
            new-target (- target coin)]
        
        (cond
          (zero? new-target) 
          result

          (empty? remaining) 
          (recur new-target dec-result (rest remaining))

          :else (recur new-target result (rest remaining))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make a coin checker

; what does it do? 
; it evaluates (- target coin). if zero, make count one. if neg, make count 0, if pos, give me the result as new target

(defn coin-checker [target coin]
  (let [diff (- target coin)
        result (cond 
                 (zero? diff) {coin (inc 0)}
                 (neg? diff) {coin 0}
                 (pos? diff) {coin (inc 0)})]
    result))

(coin-checker 17 14)
;; => {14 1}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; USING DIFFERENCE INSTEAD OF QUOTION

(defn change-bones [x coinset]

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (empty? remaining)

      result

      (let [coin (first remaining)
            _ (prn (str "coin: " coin))
            diff (- target coin)
            _ (prn (str "diff: " diff))
            new-result (cond 
                         (zero? diff) (conj result [coin (inc 0)])
                         (neg? diff) (conj result [coin 0])
                         (pos? diff) (conj result [coin (inc 0)])) ; i need to call the fn on itself in here?
            _ (prn (str "new-result: " new-result))]
        
        (recur new-result diff (rest remaining))))))

(change-bones 70 [1 10 25])
;; => {25 1, 10 1, 1 1}
;; this fn can tell me the coin and count of 1 if it can go in at least 1 time.

(defn change-bones-again [x coinset]

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (empty? remaining)

      result

      (let [coin (first remaining)
            _ (prn (str "coin: " coin))
            diff (- target coin)
            _ (prn (str "diff: " diff))
            new-result (cond 
                         (neg? diff) nil
                         (zero? diff) {}
                         :else (change-bones-again diff (rest remaining)))]

        (conj result [coin (inc 0)])
        
        (recur result diff (rest remaining))))))

(change-bones-again 70 [1 10 25])
;; => {}
;; what?


; what do i need?

; i need a machine that works thru a collection, and can explore diff branches

; i need a machine to see if a coin will fit into a target and/or how many times it fits.

; i need something to keep count of how many times a coin fits into the target.

; if i was doing it in real life, id pick up the biggests coins, then the next.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
  (zero? target) result
  (empty? remaining) ; #4 special stuff - back up and take out a coin, start over
  :else ; (in other words, if target = not 0 and remaining not empty, keep going, return / use target, and keep processing like "normal".
  ) 

(defn change-with-quot [x coinset]
"using if"

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (zero? target)

      result
      
      (let [coin (first remaining)
            _ (prn (str "coin: " coin))
            quotient (quot target coin)
            _ (prn (str "quotient: " quotient))
            new-target (rem target coin)
            _ (prn (str "new-target: " new-target))
            new-result (if (empty? remaining)
                         (update result coin dec) ; take one biggest coin away, and do it all again.
                         (conj result [coin quotient])) ; you need to call itself? do something to this thing before you conj it to the result. what do i need to do to {coin quote. how di use rem. i need to use the remainder. new target is not being used. can i make a fn outside of this fn? what kinda machine would it be? 
            _ (prn (str "new-result: " new-result))]
        
        (recur new-result new-target (rest remaining))))))
;; => #'wendy.change/change-with-quot

(change-with-quot 17 [4 9 14 25])
; Execution error (NullPointerException) at wendy.change/change-with-quot (REPL:899).
; null

"coin: 25"
"quotient: 0"
"new-target: 17"
"new-result: {25 0}"
"coin: 14"
"quotient: 1"
"new-target: 3"
"new-result: {25 0, 14 1}"
"coin: 9"
"quotient: 0"
"new-target: 3"
"new-result: {25 0, 14 1, 9 0}"
"coin: 4"
"quotient: 0"
"new-target: 3"
"new-result: {25 0, 14 1, 9 0, 4 0}"
"coin: "


steps

coin (first remaining)
quotient (quot target coin)
new-target (rem target coin)
if new-target is zero, 
   (conj result {coin quotient}). you're done. totally done. go no further. thats the answer.
    else recur
         coin (first remaining)
         quotient (quot new-target coin)
         new-target (rem new-target coin)
         if new-target is zero, 
               (conj result {coin quotient})
               else recur
                    coin (first remaining)
                    quotient (quot new-target coin)
                    new-target (rem new-target coin)
                    if new-target is zero, 
                        (conj result {coin quotient})
                         else recur  
                              coin (first remaining)
                              quotient (quot new-target coin)
                              new-target (rem new-target coin)
                              if new-target is zero, 
                                  (conj result {coin quotient})
                                   else recur...

so it seems you want to be recurring with new-target, new-result, and (rest remaining)

What's the other case?

new-result:
if (not (zero?) new-target) and (empty? remaining)
    take new-result and fix it: (update new-result (first new-result) dec)

if quotient is zero, give result without that coin and quotient.

what do we want the following to be?

remaining coinset: ALL coins except for the one thats are larger than target and the one coin / key we dec'd
new-target: as it stands 
new-result: {25 0 10 1} i think, all coins before the one we dec'd and then the one we dec'd.

; Start over with new result as described above, remaining as coll not including other coins in result, target as original

; what do you want to do to result? find the largest key, dec its val. update the key-val to this new val.
                   ; what could result look like at this point? {25 1 10 2}
                   ; step 1 find largest key or first key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


steps

coin (first remaining)
quotient (quot target coin)
new-target (rem target coin)
new-result (if (zero? new-target))
             then (if (not (zero? quotient)))
                    then (conj result {coin quotient}) ; you're done. totally done. go no further. thats the answer.
                    else (if (empty? remaining)) 
                             then (update result coin dec)
                             else recur

new-result (cond
             (zero? new-target) (if (not (zero? quotient) 
                                         (conj result [coin quotient])
                                         result))
             (empty? remaining) (update result coin dec)
             :else (if (not (zero? quotient) 
                            (conj result [coin quotient])
                         result)))

recur with new-target, new-result, and (rest remaining)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-with-quot-2 [x coinset]
"using cond"

  (loop [result {}
         target x
         remaining (reverse coinset)]
      
    (let [coin (first remaining)
          _ (prn (str "coin: " coin))
          quotient (quot target coin) ;;; NUL POINTER _- FIGURE IT OUT
          _ (prn (str "quotient: " quotient))
          _ (prn (str "target: " target))
          new-target (rem target coin)
          _ (prn (str "new-target: " new-target))
          new-result  (cond

                        (empty? remaining) ;;;;;;; ; if remaining is empty, fix the new-result and run stuff over again.
                                                  ;; what does the standard "run stuff" look like? (conj result {coin quotient}) - build a result!
                        (update result coin dec)

                        (zero? new-target) 

                        (if (not (zero? quotient))
                          (conj result [coin quotient])
                          result)
                        
                        :else (if (not (zero? quotient))
                                (conj result [coin quotient])
                                result))

          _ (prn (str "new-result: " new-result))]
      
      (recur new-result new-target (rest remaining)))))

(change-with-quot-2 17 [4 9 14 25])
;; => Execution error (NullPointerException) at wendy.change/change-with-quot-2 (REPL:1269).
;;    null

; prob: only one coin/count pair getting in

; fix that

; "coin: 25"
; "quotient: 0"
; "target: 17"
; "new-target: 17"
; "new-result: {}"
; "coin: 14"
; "quotient: 1"
; "target: 17"
; "new-target: 3"
; "new-result: {14 1}"
; "coin: 9"
; "quotient: 0"
; "target: 3"
; "new-target: 3"
; "new-result: {14 1}"
; "coin: 4"
; "quotient: 0"
; "target: 3"
; "new-target: 3"
; "new-result: {14 1}"
; "coin: "

; what do we want the following to be if i gotta dec a coin?

; remaining coinset: ALL coins except for the one thats are larger than target and the one coin / key we dec'd
; new-target: as it stands 
 ;new-result: {25 0 10 1} i think, all coins before the one we dec'd and then the one we dec'd.


(defn change-with-quot-3 [x coinset]

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (zero? target)

      result
    
        (let [_ (prn (str "remaining: " remaining))
              coin (first remaining)
              _ (prn (str "coin: " coin)) ; 
              _ (prn (str "target-b4-quot: " target))
              quotient (quot target coin) ;;; NUL POINTER - FIGURE OUT WHY, how could each be nil? which is nil and why? what is the value of each symbol? how does target get a value?
              _ (prn (str "quotient: " quotient))
              new-target (rem target coin)
              _ (prn (str "new-target: " new-target))
              new-result  (cond

                            (empty? remaining) ;;;;;;; ; if remaining is empty, fix the new-result and run stuff over again.
                            ;; what does the standard "run stuff" look like? (conj result {coin quotient}) - build a result!
                            (update result coin dec)
                            
                            :else (if (not (zero? quotient))
                                    (conj result [coin quotient])
                                    result))

              _ (prn (str "new-result: " new-result))]
          
          (recur new-result new-target (rest remaining))))))

(change-with-quot-3 17 [4 9 14 25])
;; => Execution error (NullPointerException) at wendy.change/change-with-quot-3 (REPL:1125).
;;    null

(first ())
;; => nil

(quot 17 nil)
;; => Execution error (NullPointerException) at wendy.change/eval7559 (REPL:1076).
;;    null

; according to the prns, the only kv pair getting into the {} is 14 1 which passes the if-quot-not-zero test

; null pointer: fn trying to use something that does not exist
;     where are you trying to use things that doesn't exist? 

; what is remaining here? ()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; write a fn that takes a collection and a target, goes thru coll one by one, and returns an answer of whether or not a coin will go into target

(defn divisible-by? 
  [target coin]
  (if (<= 1 (quot target coin))
    "true"
    "false"))

(divisible-by? 3 5)

(defn walk-the-coins 
  [target coinset]

  (loop [result []
         remaining coinset]

    (if (empty? remaining)

      result

      (let [coin (first remaining)
            answer (divisible-by? target coin)
            new-coll (conj result answer)]

        (recur new-coll (rest remaining))))))

(walk-the-coins 17 [4 9 14 25])
;; => ["true" "true" "true" "false"]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; write a fn that takes a collection and a target, and returns a coll of coins that go into target at least once.

;  write a fn that checks whether or not a coin fits into the target at least once, returns coll of coin or nil.

(defn coin-that-fits
  [target coin]
  (if (<= 1 (quot target coin))
    coin))

(defn coins-that-go-in
  [target coinset]

(loop [result []
       remaining coinset]

  (if (empty? remaining)

    result

    (let [coin (first remaining)
          maybe-coin (coin-that-fits target coin)
          new-coll (conj result maybe-coin)]

      (recur new-coll (rest remaining))))))

(coins-that-go-in 17 [4 9 14 25])
;; => [4 9 14 nil]

; PARTIAL SUCCESS. Better to not let nils get in.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; write a fn that takes a collection and a target, and returns a map with kv pairs of coins and counts. counts are max number of times a coin can go into target without going over.

(defn coins-with-counts
  [target coinset]

(loop [result {}
       remaining coinset]

  (if (empty? remaining)

    result

    (let [coin (first remaining)
          pair [coin (quot target coin)]
          new-coll (conj result pair)]

      (recur new-coll (rest remaining))))))

(coins-with-counts 17 [4 9 14 25])
;; => {4 4, 9 1, 14 1, 25 0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; now REVERSE the coinset before you start walking thru it, takes coll and target, returns a map with kv pairs of coins and counts.

(defn coins-with-counts
  [target coinset]

 (loop [result {}
       remaining (reverse coinset)] ; this is the initial binding of the loop


  (if (empty? remaining)

    result

    (let [coin (first remaining)
          pair [coin (quot target coin)]
          new-coll (conj result pair)]

      (recur new-coll (rest remaining))))))

(coins-with-counts 17 [4 9 14 25])
;; => {25 0, 14 1, 9 1, 4 4}

; Success! 

; What else do we need?

; 1. a condition to deal with remaining being empty.

; 2. we need the value of (quot target coin) as quotient

; 3. we need the value of (rem target coin) as new-target.

; 4. at what point do i start over or reassign the targert?

; 5. remember the conditions

; if (zero? target) result

; if (not= 0 target) and (empty? remaining) -> do #4

; if (not= 0 target) and (seq remaining) -> Keep going - Return target - keep processing like "normal"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COND :

(cond
  (zero? target) result 
  (empty? remaining) ; if true, target = not 0 and remaining empty, do work listed below in 4. if target = not 0 and remaining not empty , go to next ------->
  :else ; if target = not 0 and remaining not empty, then keep going, return / use target, and keep processing like "normal".

;; IF :

(if (zero? target)

  result

  (if (empty? remaining)

    (update result coin dec) ; dec val of largest key with non-0 val. this is the new-result to work with in future. recur again. target as initial.
    _____________ ;recur with stuff
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; x Work in conditional for (if (empty? remaining)). hint, write the condition before you use it.

(defn coins-with-counts
  [x coinset]

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (zero? target)
      result
      (if (empty? remaining) ; if target is not zero, and theres nothing left in remaining:
        (let [new-result (update result coin dec)]
      ;   new-result 
      ; bind that value to new-result
      ; loop again but we need the following
        ; our new-result that was dec'd
        ; what target?
        ; what is remaining?
      
      (let [coin (first remaining)
            quotient (quot target coin)
            new-target (rem target coin)
            redo-result (update result coin dec)
            (conj result [coin quotient])] ; building answer

        (recur new-result new-target (rest remaining))))))))

(coins-with-counts 17 [4 9 14 25])
;; unfinished


; keep track of the REMAINDER

(defn coins-quots-rems
  [target coinset]

(loop [result {}
       remaining (reverse coinset)] 

  (if (empty? remaining)

    result

    (let [coin (first remaining)
          quotient (quot target coin)
          remainder (rem target coin)
          new-coll (conj result [coin [quotient remainder]])]

      (recur new-coll (rest remaining))))))

(coins-quots-rems 17 [4 9 14 25])
;; => {25 [0 17], 14 [1 3], 9 [1 8], 4 [4 1]}

; this returns a map of kv pairs. the val vector is quotient and remainder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn change-with-quot [x coinset]
"using if"

  (loop [result {}
         target x
         remaining (reverse coinset)]

    (if (zero? target)
      result
      (if (empty? remaining)
        (let [coin (first remaining)
              _ (prn (str "coin: " coin))
              quotient (quot target coin)
              _ (prn (str "quotient: " quotient))
              new-target (rem target coin)
              _ (prn (str "new-target: " new-target))
              new-result (update result coin dec)
              _ (prn (str "new-result: " new-result))]
          
          (recur new-result new-target (rest remaining)))

        (let [coin (first remaining)
              quotient (quot target coin)]

          (conj result [coin quotient])

         (recur new-result new-target (rest remaining)))))))
;; => #'wendy.change/change-with-quot

#_(change-with-quot 17 [4 9 14 25])
;; => {25 0}, broken

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make fn that says make change or not

; Because I have a process where i have coin, quotient, remainder for all my coins
; first thing: am i done?
;                yes? give me result
;                no? do work again - maybe different work?

(defn coins-quots
  [target coinset]
  (loop [result {}
         remaining (reverse coinset)] 
    (if (empty? remaining)





      result
      (let [coin (first remaining)
          quotient (quot target coin)
          new-coll (conj result [coin quotient])]
        (recur new-coll (rest remaining)))))) 

(coins-quots 17 [4 9 14 25]) ;; => {25 0, 14 1, 9 1, 4 4}

(defn coins-quots-rems
  [target coinset]
  (loop [result {}
         remaining (reverse coinset)] 
    (if (empty? remaining)
      result
      (let [coin (first remaining)
           quotient (quot target coin)
           remainder (rem target coin)
           new-coll (conj result [coin [quotient remainder]])]
       (recur new-coll (rest remaining))))))

(coins-quots-rems 17 [4 9 14 25]) ;; => {25 [0 17], 14 [1 3], 9 [1 8], 4 [4 1]}

; no, you're not done. 

; write a way to check if your new-coll equals target?

; write a way to check to see if (true or false) any number of coins in coinset will sum to equal target.?

(vals {25 0, 14 1, 9 1, 4 4})
;; => (0 1 1 4)

(vals {25 [0 17], 14 [1 3], 9 [1 8], 4 [4 1]})
;; => ([0 17] [1 3] [1 8] [4 1])

if remainder is zero, coinset will meet target.
if quotient is greater than 0, 

(keys {25 0, 14 1, 9 1, 4 4})
;; => (25 14 9 4)

(flatten (repeat 70 [3 10])) 

;; (3 10 3 10 3 10 3 10....)

; now that i have a lot of coins, can i reduce these to a smaller set that equals the target?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUT CONSIDER BASE CASES

; each time (- target coin) is not zero or negative, increase the coin count by 1: [coin (inc 0)]
 
(defn base-cases [target coin]
  (cond
    (neg? target) nil
    (zero? target) {}
    :else (conj result [coin (inc 0)])))

(defn coins-with-base
  [x coinset]

  (loop [result {}
         target x
         remaining coinset] 

    (if (seq remaining)
      
      (let [coin (first remaining) ;; [4 9 14 25]
            new-target (- target coin) ;; 
            new-coll (cond
                      (neg? new-target) (coins-with-base x coinset)
                      (zero? new-target) "done"
                      :else (conj result [coin (inc 0)]))]
        (recur new-coll new-target (rest remaining)))

      result))) 

(coins-with-base 17 [4 9 14 25])
;; => Execution error (StackOverflowError)
;;    null

(coins-with-case 5 [1 1 1 1 1])
;; => "done"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

what do you have?

   x = an amount you need to make change for

   coinset = a collection of coins available to you, in whatever amount you need of each coin

how would you solve this in real life?

  1. Take as many of the largest coin from x without going in the negative. Put those coins in a pile.
  2. Take as many of the next largest coin from the now smaller-x without going negative. Put those coins in a pile.
  3. Take as many of the next largest coin from the smaller-x without going negative. Put those coins in a pile.
  4. Repeat until smaller-x is zero. In other words, stop when smaller-x is zero.

can you subtract coins in coinset one by one from x until you have nothing?

   get coin from coinset.

   subtract coin from x (- x coin). this is now smaller-x.

   if (pos? smaller-x)
       do this fn over again (getting a coin, (- smaller-x coin))

;;;;;;;;;;;;;;;;;;;;;;;;

can i be creative in how i solve this problem?

can i throw coins up in the air and which ever fall first i use and try to make a solution?

can i build a solution from just the even coins?

can i build a solution from just the odd coins?

can i skip every other can and try to make a solution from those coins?

)

(defn coins-with-base
  [x coinset]

  (loop [result {}
         target x
         remaining coinset] 

    (if (seq remaining)
      
      (let [coin (first remaining) 
            new-target (- target coin)
            new-coll (cond
                      (neg? new-target) (coins-with-base x coinset)
                      (zero? new-target) [coin 1]
                      :else (conj result [coin (inc 0)]))]
        (recur new-coll new-target (rest remaining)))

      result))) 

(coins-with-base 70 [3 10])
;; => {3 1, 10 1}
;; wrong answer
;; look at where its giving you an answer. its the wrong info or wrong place.
;; why?
;; when?
;; wrong time
;; wrong reason

; theres three things im looping with
; what are all (9?) combos
; what do you do in each one?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how do I make 1 coin work with a target?

; target = 70
; coinset = [1]

(quot x (first coinset))
(quot 70 1) ;; => 70

{(first coinset) (quot target (first coinset))}3
;; {1 70}  which is also in this case : {coin target}

(flatten (repeat 70 [3 10])) 

(flatten (repeat x coinset))

(defn coins-base-repeat
  [x coinset]

  (loop [solution {}
         target-amount x
         purse (flatten (repeat x coinset))] 

    (if (seq purse)
      
      (let [
            _ (prn (str "purse: " purse))
            coin (first purse) 
            _ (prn (str "coin: " coin))
            new-target-amount (- target-amount coin)
            _ (prn (str "new-target-amount: " new-target-amount))
            new-coll (cond
                      (neg? new-target-amount) nil
                      (zero? new-target-amount) [coin (inc 0)]
                      :else (conj solution [coin (inc 0)]))
            _ (prn (str "new-coll: " new-coll))]
        (recur new-coll new-target-amount (rest purse)))

      solution))) 

(coins-base-repeat 3 [1 2])
;; => nil

; make sequence of coins.
; what do i want to do with that?
; pick up all the ones that sum to make target.
; count all the same coins.

; figure out how to iron the whole sheet at once.

; by picking up each coin individually, im ironing each piece (and wasting time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RANDOM TIME!

;; how can i use randomness to solve my problem? 

;; Create fn that assigns random ints as counts of coins.

(defn generate [x coin-set] 
  (zipmap coin-set (repeatedly (fn [] (rand-int x)))))

(generate 10 [1 2 3 5])
;; => {1 8, 2 3, 3 4, 5 9}
;; SUCCESS

;; Create fn that checks if a possible solution is a valid solution.

(defn valid? [x solution] 
  (= x (apply + (map (fn [[k v]] (* k v)) solution))))

;; Create fn that generates possible solutions until it reaches a valid solution.

(defn generate-til-valid [x coin-set] ; using loop instead of let
  (loop [solution (generate x coin-set)] ; with loop, i could keep track of another value if wanted
    (if (valid? x solution)
      solution
      (recur (generate x coin-set)))))

(comment

if valid? is true, result
if valid? is false, keep generating random solutions
)

(defn generate-til-valid [x coin-set] ; using let instead of loop
  (let [solution (generate x coin-set)] 
    (if (valid? x solution)
      solution
      (recur x coin-set)))

(valid? 10 {1 8, 2 3, 3 4, 5 9})

)

;; TODO:

;; generate fn 

;; 1.  get coin from coinset. use let.

;; 2.  use rand-int of (quot x coin) instead of x (plus 1 to include the quot. this caps the rand-int to (quot x coin).

;; 3.  use map instead of zipmap to generate a hash map step by step.

;; 4.  bail on answer after 10,000 tries if no answer

;;;;;;;

;; 1. get coin from coinset.

(let [coin (first coin-set)]
  coin)

;; 2. change exclusive n to rand-int.

(rand-int (+ 1 (quot 10 5))) ;; => the result will be 0, 1, or 2. The result of (quot x coin) here is 2.

;; 3. use map instead of zipmap to generate a hash map step by step.

;; whatcha got? i have a collection and I need to process it.

;; (map f coll)

;; whatcha need? a fn to process my coll. What do I need my fn to do?

;; turn my coll into a hash map: coins are keys, rand-ints are vals.

;; write a fn for map that generates rand-ints based on my coins, no larger than quot

(map (fn [coin] (rand-int (+ 1 (quot 10 coin)))) [1 2 3 5])
;; => (8 3 3 2)
;; these are my vals

;; now make the above fn put the coins as keys and the above items as vals.

(mapcat hash-map 
     [1 2 3 5] ;; coll of my coins i want to be keys
     (map (fn [coin] (rand-int (+ 1 (quot 10 coin)))) [1 2 3 5])) ;; coll of rand-ints i want to be vals
;; => ([1 3] [2 4] [3 2] [5 0])

(map hash-map 
     [1 2 3 5] ;; coll of my coins i want to be keys
     (map (fn [coin] (rand-int (+ 1 (quot 10 coin)))) [1 2 3 5])) ;; coll of rand-ints i want to be vals
;; => ({1 10} {2 2} {3 2} {5 2})


(comment 

;;;;; USING MAP

(map f coll)

(map f coll coll)

; just do the work in your anon fn. what work?

; map __?____  over your coinset

; what do you want to map over your coinset?

; random vals made from using rand-int


;;;;; USING REDUCE

; what do you want?

; change my coinset into a map with vals associated with my coinset items (coins)

; a hash-map: the keys are coins from coinset, the vals are the rand-vals from (map (fn [coin] (rand-int (+ 1 (quot x coin)))) coinset)

(reduce f coll)

; f takes two arguments

(reduce f val coll)

;  Returns the result of applying f to val and the first item in coll, then applying f to that result and the 2nd item, etc.

;  What f can I apply to val and first item in coll?

(defn rand-val-generator
 [x coin]
  (rand-int (+ 1 (quot x coin))))

(rand-val-generator 10 5)
;; => 1

(reduce rand-val-generator {} [1 2 3 5])
;; =>    class clojure.lang.PersistentArrayMap cannot be cast to class java.lang.Number 

(map (fn [coin] (rand-int (+ 1 (quot 10 coin)))) [1 2 3 5])
;; => (1 5 3 1) ;; these are my rand vals

(reduce hash-map
     [1 2 3 5] ;; coll of my coins i want to be keys
     (map (fn [coin] (rand-int (+ 1 (quot 10 coin)))) [1 2 3 5])) ;; coll of rand-ints i want to be vals
;; => {{{{[1 2 3 5] 8} 4} 2} 0}

; what does just the f part look like?
; make coins ur keys...


(reduce (fn [x coin]
          (rand-int (+ 1 (quot x coin))))
        {}
        [1 2 3 5])
;; =>    class clojure.lang.PersistentArrayMap cannot be cast to class java.lang.Number 
;; =>    init val of x is {}, and quot is expecting a number, so this doesn't work.


(reduce (fn [acc nxt] ; init val of acc is 2nd arg to reduce {}, then its last return val of reducing fn (the anon fn).
                      ; nxt is 1st item of supplied coll

          (assoc acc nxt (rand-int (+ 1 (quot 10 nxt))))) 
        {} 
        [1 2 3 5])
;; => {1 4, 2 1, 3 3, 5 2}
;; => {1 5, 2 0, 3 1, 5 2}
;; => {1 5, 2 5, 3 3, 5 1}
;; => {1 10, 2 1, 3 1, 5 1}


;;;;;;;  MY NEW GENERATE FN

(defn generate-w-reduce
  [target coin-set]
  (reduce (fn [acc nxt]
            (assoc acc nxt (rand-int (+ 1 (quot target nxt))))) 
        {} 
        coin-set))

(generate-w-reduce 10 [1 2 3 5])
;; => {1 3, 2 3, 3 2, 5 2}
;; This fn produces a collection of randomly generated counts for each coin, with a count no larger than result of (quot x coin)


;;;;;;;  MY OLD VALID? FN

(defn valid? [x solution] 
  (= x (apply + (map (fn [[k v]] (* k v)) solution))))


;;;;;;;  MY OLD GENERATE-TIL-VALID FN

(defn generate-til-valid [x coin-set] 
  (loop [solution (generate-w-reduce x coin-set)] ; with loop, i could keep track of another value if wanted
    (if (valid? x solution)
      solution
      (recur (generate-w-reduce x coin-set)))))

(generate-til-valid 10 [1 2 3 5])
;; => {1 3, 2 1, 3 0, 5 1}
;; SUCCESS :)


;;;;; REWRITE VALID?

(defn valid?
  [x solution]
  (= x (apply + (map (fn [[coin count]] (* coin count)) solution))))

(valid? 5 {1 3, 2 1})
;; => true
(valid? 10 {1 8, 2 3, 3 4, 5 9})
;; => false
(valid? 10 {1 3 2 3 5 1 10 1})
;; => false


;;;;;; UPDATE THIS FN to bail after 10,000 tries

; what do i need?

; i need to know if the coinset will not return change for x.

; i need to stop recursion if i do not retrieve an answer after 10,000 tries.

; how do i get it?

; keep track of how many times i recur and terminate after a certain limit is reached.

; how do i keep track of how many times i recur?  inc a count

(defn generate-til-valid [x coin-set] ; with loop, i could keep track of another value if wanted
  (loop [solution (generate-w-reduce x coin-set)
         tries 0] ; 0 is init binding 
    (if (valid? x solution) ; if true,
      solution ; return the solution
      (if (< tries 10000) ; if false
        (recur (generate-w-reduce x coin-set) (inc tries)) ; recur: generating another solution and inc'ing tries
        "Nope, can't make change")))) ; if tries > 10,000

(generate-til-valid 17 [4 9 14 25])
;; => {4 2, 9 1, 14 0, 25 0}

(generate-til-valid 10 [3 14])
;; => "Nope, can't make change"

(generate-til-valid 129 [1 128])
;; => {1 1, 128 1}

(generate-til-valid 129 [129])
;; => {129 1}

;; cool, but zeros are part of coll which is not cool.

; i want to return kvs where v is not 0

; don't let kv pairs in with 0 for count

; alter the generative fn: if count (the val) is zero, don't assoc that kv pair. give me the acc instead.

(defn generate-sin-zeros
  [x coin-set]
  (reduce (fn [acc nxt]
            (if (pos? (rand-int (+ 1 (quot x nxt)))) 
              (assoc acc nxt (rand-int (+ 1 (quot x nxt))))
              acc))
        {} 
        coin-set))

;; uh, you're calling rand-int twice...

(generate-sin-zeros 17 [4 9 14 25])
;; => {4 0, 14 1}
;; => {9 0}
;; => {4 2, 14 0}
;; FAIL

; how bout try to filter them out?

(defn generate-reduce-filter
  [x coin-set]
  (into {} (filter (fn [[k v]] (pos? v))
                   (reduce (fn [acc nxt]
                             (assoc acc nxt (rand-int (+ 1 (quot x nxt)))))
                           {} 
                           coin-set))))

(generate-reduce-filter 17 [ 4 9 14 25])
;; => {4 2, 9 1}

;; SUCCESS!

(defn generate-til-valid [x coin-set] ; with loop, i could keep track of another value if wanted
  (loop [solution (generate-reduce-filter x coin-set)
         tries 0] ; 0 is init binding 
    (if (valid? x solution) ; if true,
      solution ; return the solution
      (if (< tries 10000) ; if false
        (recur (generate-reduce-filter x coin-set) (inc tries)) ; recur: generating another solution and inc'ing tries
        "Nope, can't make change"))))

(generate-til-valid 17 [4 9 14 25])
;; => {4 2, 9 1}

(generate-til-valid 10 [1 2 3 5])
;; => {1 4, 2 3}

(generate-til-valid 6 [5 7])
;; => "Nope, can't make change"

;; um, you frickin' did it? but can you do it better without a filter?

; what do you want your reducing fn to do?
; create a rand-int
; if pos?
; associate the accumulator with the first item of coll and the rand-int generated with a collection
; otherwise gimme acc

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; nov 9 6:30 pm

; todo ideas:
; [x] refactor possible-solution with map and into, instead of reduce
; [ ] refactor make-change with a let with just tries (no loop, no solution in binding)

; [ ] other options: generate a bounded list of all solutions and filter for optimal

(comment 

(make-change 1000000 [1])
;; => {1 1000000} ;; got lucky! it is possible tbat make-change would miss this.
) 

;;;; refactor possible-solution with map and into, instead of reduce

; [] what do you want your map fn to do? 

; generate a rand-int between 0 and one more than the quot of coin and target.

; create a hashmap, using coinset items as ks and rand-counts as vs.

(defn rand-count-maker
  [target coinset]
  (map (fn [coin] 
       (rand-int (inc (quot target coin))))
       coinset))

(defn possible-solution-with-zeros
  [target coinset]
  (into {} 
        (map vector
             coinset
             (rand-count-maker target coinset))))


(defn valid?
  [target solution]
  (= target (apply + (map (fn [[coin count]] (* coin count)) solution))))


(defn make-change
  [target coin-set]
  (loop [solution (possible-solution target coin-set)
         tries 0]
    (if (valid? target solution)
      (do (prn tries)  
          solution)
      (when (< tries 10000)
        (recur (possible-solution target coin-set) (inc tries))))))


(defn rand-solution-maker
  [target coinset]
  (->> coinset
       (map (fn [coin]
              [coin (rand-int (inc (quot target coin)))]))
       (into {})))

(rand-solution-maker 10 [1 2 3 5])
;; => {1 0, 2 5, 3 2, 5 2}

; zipmap

(comment

(map f coll)

(map vector 
     [1 2 3 5] 
     (rand-count-maker 10 [1 2 3 5]))
;; => ([1 4] [2 0] [3 1] [5 2])

(into {} *1)
;; => {1 4, 2 0, 3 1, 5 2}

(rand-count-maker 10 [1 2 3 5])
;; => (5 2 3 1)
;; => (4 0 1 2)

(possible-solution-with-zeros 10 [1 2 3 5])
;; => {1 7, 2 1, 3 1, 5 0}
;; => {1 2, 2 2, 3 0, 5 2}

(make-change 10 [1 2 3 5])
;; => {1 5, 2 1, 3 1}

(valid? 10 {1 7, 2 1, 3 1, 5 0})
)

; refactor make-change with a let with just tries (no loop, no solution in binding)

(defn make-change
  [target coin-set]
  (loop [tries 0]
    (let [solution (possible-solution-with-zeros target coin-set)]
      (if (valid? target solution)
        solution
        (when (< tries 10000)
          (recur (inc tries)))))))

(make-change 10 [1 2 3 5])
;; => {1 1, 2 2, 3 0, 5 1}
;; => {1 1, 2 0, 3 3, 5 0}


; write a fn that generates a possible solution and tests whether or not it is a valid solution.

(defn rand-counts
  [target coinset]
  (map (fn [coin] 
       (rand-int (inc (quot target coin))))
       coinset))

(defn rand-solution
  [target coinset]
  (into {} 
        (map vector
             coinset
             (rand-counts target coinset))))

(defn validation-machine
  [target coinset]
  (let [solution (rand-solution target coinset)]
    (do (prn solution)
        (= target (apply + (map (fn [[coin count]] (* coin count)) solution))))))

(comment 

(validation-machine 10 [1 2 3 5])

)

; what do you need now?

; for this function to run over and over again until true, if it is possible to return true.

; what is another option?

;;;;;;;;;;;; Generate a bounded list of all solutions and filter for optimal

(comment

; write an example of fn call and collection of tuples if we make all possible combinations

(make-change 17 #{4 9 14 25}) [[4 1] [9 1] [14 1] [25 1]] [[4 2][9 1] [14 0] [25 0]] [[4 3] [9 1] [14 1] [25 1]] [[4 4][9 1] [14 1] [25 1]]...

; what are all the possible combinations for the given target and coinset? 

; limit the max count to result of (quot target coin)

target = 3
coinset = [1 3]

all possible solutions:

;; as maps

({1 0, 3 0} ; this could be the starting coll or count
 {1 1, 3 0}
 {1 2, 3 0}
 {1 3, 3 0}

 {1 0, 3 1}
 {1 1, 3 1}
 {1 2, 3 1}
 {1 3, 3 1})

{1 0, 3 2} ; program should bail instead of producing this one.
......

;; as vectors of tuples

([[1 0] [3 0]]
 [[1 1] [3 0]]
 [[1 2] [3 0]]
 [[1 3] [3 0]]

 [[1 0] [3 1]]
 [[1 1] [3 1]]
 [[1 2] [3 1]]
 [[1 3] [3 1]])

[[1 0] [3 2]] ; program should bail instead of producing this one.

; generate collection of each possiblle amount of coins available, based on (quot target coin)

; what ya got there?

1. collection of tuples of integers.
2. what do i want to do with those?

what would a smart ass do if you told them what you think you wanted?
would the result be what u were looking for?

whats the smallest thing you can change about this statement to make it a little more right, a little less wrong?

what are you describing right now?

engineering - small logicial steps

; generalize specificity

(reduce (fn [acc nxt] 
          (assoc acc nxt (range (inc (quot 3 nxt)))))
        {}
        [1 3])
;; => {1 (0 1 2 3), 3 (0 1)}
;; uh, no.

; what do i want to keep track of?

; if ive finished my coinset
; if the range has been completed

; outer loop iterates over the coinset [1 3]
; inner loop iterates over the range (0 1 2 3)

;; don't be scared - just do it - F*** IT UP!

(defn make-it-a-map
  [x]

  (loop [remaining x
         result {}]

    (if (empty? remaining)
      
      result

      (let [primero (first remaining)
            map-result (assoc result primero 0)]
        
        (recur (rest remaining) map-result)))))

(make-it-a-map [1 3])
;; => {1 0, 3 0}

;; what else do i need?

(range (inc (quot 3 1)))
;; => (0 1 2 3)

;; map each of these to the 1 coin.

(range (inc (quot 3 3)))
;; => (0 1)

;; map each of these to the 3 coin.

;;;;;;; start here

;;;; below doesnt work

(defn solution-maker-reduce 
  [target coinset]
  (reduce (fn [acc nxt] 
            (assoc acc nxt (range (inc (quot target nxt)))))
          {}
          coinset))

(solution-maker-reduce 3 [1 3])
;; => {1 (0 1 2 3), 3 (0 1)}
;; nope

(defn solution-maker
  [target coinset]
  (->> coinset
       (map (fn [coin]
              [coin (range (inc (quot target coin)))]))
       (into {})))

(solution-maker 3 [1 3])
;; => {1 (0 1 2 3), 3 (0 1)}
;; nope

;; what is wrong with this?

;; 1. result is one hashmap, rather than a collection of hashmaps. i wanted a coll of hashmaps.
;; 2. vals are collections, rather than ints from the collection.

;; ok rethink this
;; coinset = [1 3]
;; target = 3
;; what do I need? 
;;  all possible counts for 1
;;  all possible counts for 3

(defn coin-counts
  [target coin]
  (range (inc (quot target coin))))

(coin-counts 3 1)
;; => (0 1 2 3)

(coin-counts 3 3)
;; => (0 1)

;; first item of coinset is k1 > first item of k1-poss-count is v1 > kv pair goes into its own map. {1 0}
;; next item of coinset is k2 > first item of k2-poss-count is v2 > conj kv pair to existing map. {1 0, 3 0}
;; this map added to final result. ({1 0, 3 0})

;; first item of coinset is k1 > second item of k1-poss-count is v1 > kv pair goes into its own map. {1 1}
;; next item of coinset is k2 > first item of k2-poss-count is v2 > conj kv pair to existing map. {1 1, 3 0}
;; this map added to final result. ({1 0, 3 0} {1 1, 3 0})

;; first item of coinset is k1 > third item of k1-poss-count is v1 > kv pair goes into its own map. {1 2}
;; next item of coinset is k2 > first item of k2-poss-count is v2 > conj kv pair to existing map. {1 2, 3 0}
;; this map added to final result. ({1 0, 3 0} {1 1, 3 0} {1 2, 3 0})

;; first item of coinset is k1 > fourth item of k1-poss-count is v1 > kv pair goes into its own map.
;; next item of coinset is k2 > first item of k2-poss-count is v2 > conj kv pair to existing map.
;; this map added to final result.

;; when k1-poss-counts is empty, do it all again and use second item of k2-poss-count for v2.

;; first item of coinset is k1 > first item of k1-poss-count is v1 > kv pair goes into its own map.
;; next item of coinset is k2 > second item of k2-poss-count is v2 > conj kv pair to existing map.
;; this map added to final result.

;; first item of coinset is k1 > second item of k1-poss-count is v1 > kv pair goes into its own map.
;; next item of coinset is k2 > second item of k2-poss-count is v2 > conj kv pair to existing map.
;; this map added to final result.

;; first item of coinset is k1 > third item of k1-poss-count is v1 > kv pair goes into its own map.
;; next item of coinset is k2 > second item of k2-poss-count is v2 > conj kv pair to existing map.
;; this map added to final result.

;; first item of coinset is k1 > fourth item of k1-poss-count is v1 > kv pair goes into its own map.
;; next item of coinset is k2 > second item of k2-poss-count is v2 > conj kv pair to existing map.
;; this map added to final result.

;; when k2-poss-counts is empty and coinset is empty, return final result.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; so what needs to happen?
;; keep track of coinset.
;; keep track of coin-counts.
;; lets call coinset coins-remaining.
;; lets call coin-counts counts-remaining.
;; lets call () result.
;; start making maps:

;; { k1: (first coins-remaining) and v1: (first (coin-counts 3 1)), k2 (next coinset) and v2: (first (coin-counts 3 3)) }
;; add this map to result.
;; repeat with following: k1 same, v1 is (next (coin-counts 3 1)) until empty, k2 (next coinset), v2: (first (coin-counts 3 3))
;; add each map to result.
;; when (coin-counts 3 1) is empty, do it all again and use v2: (next (coin-counts 3 3)) until empty.
;; when (coint-counts 3 3) is empty, give me the result.
;; when coins-remaining is empty, give me the result.

;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11-27-23

; travel thru coinset, one coin at a time, assigning v1 as first of k1-coin-count to k1 and v2 as first of k2-coin-count to k2.
; when coinset is empty, start over with coinset, using next item in k1-coin-count for v1, original item in k2-coin-count for v2.
; continue until k1-coin-count is empty.
; when k1-coin-count is empty, start over doing same work as before, using coin-set, k1-coin-counts, k2-coin-counts, but this time, travel thru k2-coin-count until empty.
; when k2-coin-count is empty, return the result.





;; wip

;; in your recur, call the main fn.

(defn something
  [target coinset]

  (loop [result ()
         coins-remaining coinset]

    (if (empty? coins-remaining)
      
      result
      
      (let [coin (first coins-remaining)
            counts (coin-counts target coin)]
        {coin (first counts)} ;;;
        (recur (something )))))) ;;;

(something 3 [1 3])
;; => (0 1 2 3)


; if you have multiple loops, make the tiniest inner one first.

; get the info it needs and update one coin.

; start with zero'd out map.
; seed ur function with the zero'd out map: {1 0, 3 0}
; update a coin-count once.

(update {1 0, 3 0} 1 inc)
;; => {1 1, 3 0}

; so what does ur fn need?

; map, key, fn

; write a fn that uses those things

; what map?  {1 0, 3 0}
; what key? one of keys from the supplied map.
; what fn? you need to supply a fn that performs the action you need

; what needs to happen with {1 0, 3 0}?

; i need to update one of the keys to the next item in (0 1 2 3)

; i need a way to take the nth item in the coll and update the key with that.

(range (inc (quot 3 1)))
;; => (0 1 2 3)

(nth '(0 1 2 3) 1)
;; => 1

(update m k f)

(update {1 0, 3 0} 1 (fn [x] (nth '(0 1 2 3) 1)))
;; => {1 1, 3 0}

(assoc m k v)

(assoc {1 0, 3 0} 1 (nth '(0 1 2 3) 1))
;; => {1 1, 3 0}

; how do you process a collection? map, reduce...

(map f coll)

(map (fn [coin] [coin (range (inc (quot 3 coin)))]) [3 1])
;; => ([3 (0 1)] [1 (0 1 2 3)])
;; hmm, i dont want the second item in the tuple to be a collection. but I can modify this fn to produce something useful.

(map (fn [count] [1 count]) '(0 1 2 3))
;; => ([1 0] [1 1] [1 2] [1 3])

(map (fn [count] [1 count]) (range (inc (quot 3 1))))
;; => ([1 0] [1 1] [1 2] [1 3])
;; result is all all kv pairs for coin 1, where each tuple is [coin-1 possible-count]

(map (fn [count] [3 count]) (range (inc (quot 3 3))))
;; => ([3 0] [3 1])
;; result is all all kv pairs for coin 3, where each tuple is [coin-2 possible-count]


; what do i want?

; collection of maps:

; first round of maps:

; 1st kv pair: first item from ([1 0] [1 1] [1 2] [1 3]) until empty, 2nd kv pair: first item from ([3 0] [3 1])

; second round of maps: 

; 1st kv pair: first item from ([1 0] [1 1] [1 2] [1 3]) until empty, 2nd kv pair: next item from ([3 0] [3 1])

; sure but lets start over:

; look at ur projected collection of collections: you want a collection of maps. lets try map to create it. what pattern do you see in ur projected coll? values are a range. since you're building maps, you'll probably need assoc. what does assoc need? map k v

; to create first set of maps with first item in coinset:

(map (fn [x] (assoc {1 0, 3 0 } 1 x)) (range 4))
;; => ({1 0, 3 0} {1 1, 3 0} {1 2, 3 0} {1 3, 3 0})

(map (fn [x] (assoc {1 0, 3 0 } 1 x)) (range (inc (quot 3 1))))

; result is:

({1 0, 3 0} 
 {1 1, 3 0} 
 {1 2, 3 0} 
 {1 3, 3 0})

(map (fn [x] (assoc {1 0, 3 0 } 3 x)) (range (inc (quot 3 3))))

; result is:

({1 0, 3 0} 
 {1 0, 3 1})

; to create next set of maps with next item in coinset:

;;;;;;;;

;(map (fn [x] (assoc {1 0, 3 0} 1 x)) (range (inc (quot 3 1))))
(map (fn [x] (assoc {1 0, 3 0} 3 x)) (range (inc (quot 3 3))))

(defn update-key-for-range
  [m k r] 
   (map (fn [x] (assoc m k x)) (range r)))

(update-key-for-range {1 0, 3 0} 3 (inc (quot 3 3)))
;; => ({1 0, 3 0} {1 0, 3 1})

(update-key-for-range {1 0, 3 0} 1 (inc (quot 3 1)))
;; => ({1 0, 3 0} {1 1, 3 0} {1 2, 3 0} {1 3, 3 0})

(map (fn [x] 
       (update-key-for-range 
        (assoc {1 0, 3 0} 3 x) 1 4)) ; this is the hashmap update-key-for-range needs
     (range (inc (quot 3 3))))
;; => (({1 0, 3 0} {1 1, 3 0} {1 2, 3 0} {1 3, 3 0})
;;     ({1 0, 3 1} {1 1, 3 1} {1 2, 3 1} {1 3, 3 1}))

(flatten *1)
;; => ({1 0, 3 0}
;;     {1 1, 3 0}
;;     {1 2, 3 0}
;;     {1 3, 3 0}
;;     {1 0, 3 1}
;;     {1 1, 3 1}
;;     {1 2, 3 1}
;;     {1 3, 3 1})

; what do i need to keep track of?

; coinset
; range of counts - what do i need for this? a coin from coinset and target

(let [sumpin 5
      other 2]
  (* sumpin other))

(loop [some binding
       other binding]
  (recur some other))

(defn seed-map
  [coinset]
  (->> coinset
       (map (fn [coin] [coin 0]))
       (into {})))

(seed-map [1 3])
;; => {1 0, 3 0}
; boo ya!

(defn do-stuff
  [coinset target] ; for example: [1 3], 3

  (loop [remaining-coins coinset ; init binding is [1 3]
         result (seed-map coinset)] ; init binding is  {1 0, 3 0} 

    (if (empty? remaining-coins) ; if no more coins in coinset,

      result ; return the result map. otherwise, do work:

      (let [coin (first remaining-coins) ; 1
            count-range (range (inc (quot target coin))) ; (0 1 2 3)
            _ (prn (str "count-range: " count-range))
            new-coll (if (empty? count-range) ; if count-range is empty,
                       result ; assign new-coll to result. otherwise, do work:
                       (map (fn [x] ; map anon fn over count-range
; problem: result is a seq here
                              (assoc result coin x)) ; assoc 1key of {1 0 3 0} with x of ; PROBLEM
                            count-range))] ; (0 1 2 3)
            ; assign new-coll to ;; => ({1 0, 3 0} {1 1, 3 0} {1 2, 3 0} {1 3, 3 0})
        (recur (rest remaining-coins) new-coll)))))

(do-stuff [1 3] 3)

; doesnt work.
; theorize why doesnt work,
; try again, simpler though

(range (inc (quot 3 1)))
;; => (0 1 2 3)

(map (fn [x] (assoc {1 0 3 0} 1 x)) (coin-counts 3 1))
;; => ({1 0, 3 0} {1 1, 3 0} {1 2, 3 0} {1 3, 3 0})
; first set of results

; make a fn that
; takes a map of any size 
; uses coins from your coinset for keys (do not hard code keys)

; write a simple loop that does a simple thing first.

(defn inc-items
  [x]
  
  (loop [remaining x
         result []]

    (if (empty? remaining)

      result

      (let [coin (first remaining)
            bigger-coin (inc coin)
            incd-coll (conj result bigger-coin)]
        (recur (rest remaining) incd-coll)))))

(inc-items [1 3])
;; => [2 4]

)



