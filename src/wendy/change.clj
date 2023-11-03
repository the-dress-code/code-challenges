(ns wendy.change
  (:gen-class))

(defn make-change [x coinset]
  {})

(comment 
  (println (make-change 6 [1 5 10 25])) ;; => {:5 1 :1 1} is the how we want our data to look
  (println (make-change 6 [3 4]))
  (println (make-change 6 [1 3 4]))
  (println (make-change 6 [5 7]))
  (println (make-change 16 [5 7 9]))
)

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



; make sequence of coins.
; what do i want to do with that?
; pick up all the ones that sum to make target.
; count all the same coins.

; figure out how to iron the whole sheet at once.

; by picking up each coin individually, im ironing each piece (and wasting time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RANDOM TIME!

;; how can i use randomness to solve my problem? can i make random counts of coins?

(defn generate [x coin-set] 
  (zipmap coin-set (repeatedly (fn [] (rand-int x)))))

(generate 10 [1 2 3 5])
;; => {1 8, 2 3, 3 4, 5 9}

(defn valid? [x solution] 
  (= x (apply + (map (fn [[k v]] (* k v)) solution))))

; generate random solutions and check to see if valid

(defn generate-til-valid [x coin-set]
  (loop [solution (generate x coin-set)] ; with loop, i could keep track of another value if wanted
    (if (valid? x solution)
      solution
      (recur (generate x coin-set)))))

(comment

if valid? is true, result
if valid? is false, keep generating random solutions
)

(defn generate-til-valid [x coin-set]
  (let [solution (generate x coin-set)] 
    (if (valid? x solution)
      solution
      (recur x coin-set))))

; modify generate to (quot x coin) for each coin - map, no zipmap. need the coin. generate map step by step.

; will blow up if no answer (bail after 10,000) bc no way to say if no answer

(defn generate [x coin-set] 
  (let [coin (first coin-set)]
    (zipmap coin-set (repeatedly (fn [] (rand-int (quot x coin)))))))

(generate 10 [1 2 3 5])

;; i have a collection and I need to process it.

(map f coll)

;; what do i need my fn to do?

;; turn a collection into a map: coins are keys, rand-ints (no larger than quot) are vals

;; write a fn for map that generates rand-ints based on my coins, no larger than quot

(map (fn [coin] (rand-int (quot 10 coin))) [1 2 3 5])
;; => (3 2 0 0)

;; now make the above fn put the coins as keys and the above items as vals.
