(ns wendy.change
  (:gen-class))

(defn make-change [x coinset]
  {})

(println (make-change 6 [1 5 10 25])) ;; => {:5 1 :1 1} is the how we want our data to look
(println (make-change 6 [3 4]))
(println (make-change 6 [1 3 4]))
(println (make-change 6 [5 7]))
(println (make-change 16 [5 7 9]))

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

; SUNDAY - now go back and try again:

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

; x can you refactor to take work out of recur - recur is doing too much, too complicated

(defn simple-nest-process [x]

  (loop [result [] ; binding
         remaining x] ; binding

    (if (empty? remaining) ; 

      result

      (let [first-item (first remaining)
            new-coll (if (coll? first-item)
                     (conj result (simple-nest-process first-item))
                     (conj result first-item))]

        (recur new-coll (rest remaining)))))) ; learn: recur dont return (a value)

(simple-nest-process [1 2 [4] 4])
;; => [1 2 [4] 4]

; ? can you write a fn to inc-the-evens?

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

; ? can you work inc-the-events into simple-nest-process?

; ?  insert the work to check if first-item is even. inc if even, otherwise, give me first-item.

(defn simple-nest-process [x]

  (loop [result [] ; binding
         remaining x] ; binding

    (if (empty? remaining) ; 

      result

      (let [first-item (first remaining)
            new-coll (if (coll? first-item)
                     (conj result (simple-nest-process first-item))
                     (conj result first-item))] ;;;;;;;;;;;;;;;;::::::::: DO IT HERRREEEEE

        (recur new-coll (rest remaining))))))





(comment 

;; Analyze nest-inspector line by line.

; - every time i do something, say what i am doing with the result of that thing.

; - the answer is never to the right.

; - for example, what are you doing with the result of let?  im returning it as the val of my fn

; - imagine each fn is replaced with its value. what are you doing with that value?

; - everything is a black box - what are you doing with the result of the black box?

;  -  be vigilant of what im actually doing vs what i want to be doing = this is where bugs live

(prn "------START OVER------")

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; BACK TO THE ACTUAL PROBLEM ;;;;;;;;;;;;;;;;;

;; What do you have? 

; - target x

; - ascending coinset

;; What is the goal?

; - Return a map such that each key is the coin, and each value is the number of times you need that coin. 

; - If x = 17 and coinset = [4 9 14 25], possible solutions are {:4 2, :9 1, :14 0, :25 0}

;; What information do I need to accomplish / what do I want?

; - My coinset is sorted. (notes say it is)

; - How is my coinset sorted? I want it descending so I fetch largest coins first. (reverse coinset)

; - How many times does the first coin of the sorted coinset go into the target?  (quot target coin); {coin (quot target coin)}

; - I need to know the remainder of original target and first coin of sorted coinset. result of (rem target coin) will be our new target for next coin.

; - How many times does the first coin of the sorted coinset go into the target? 

; - Does the target currently equal 0?

; - Is the sorted coinset empty?

;; What should we do when the following cases are true?

; - Target = 0, Remaining = empty ; I'm done - Return result

; - Target = 0, Remaining = not empty ; I'm done - Return result

; - Target = not 0, Remaining = not empty ; Keep going - Return target - keep processing like "normal"

; - Target = not 0, Remaining = empty ; Keep going but with extra work :

; -------------------------------------  In the result so far, dec the val of largest key with a non-0 val. This is now your new result.

; -------------------------------------  Start over with new result as described above, remaining as coll not including other coins in result, target as original

(comment

(let [initial []
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

             result) ; what do you want to do to result? find the largest key, dec its val.
                     ; what could result look like at this point? {24 1 10 2}
                     ; step 1 find largest key or first key
         

         remaining)))))


; end comment
)

(-> {25 2 10 2}
    first
    val
    dec)
;; => 1

; or 

(dec (val (first {25 2 10 2})))
