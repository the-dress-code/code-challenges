(ns provider
  (:require [coast]
            [components :refer [container tc link-to table thead tbody td th tr button-to text-muted mr2 dl dd dt submit input label]]
            [clj-time.core :as t]))


(defn build
"Takes provider-id and datetimes available; returns map with provider's availablity"

  [id st1 st2 st3 st4]

  {:provider-id id
   :availablity-day-1 {:start-time st1
                       :end-time st2}
   :availablity-day-2 {:start-time st3
                       :end-time st4}})

(comment 

  (build 1 "2023-10-17 08:00:00" "2023-10-17 17:00:00" "2023-10-18 07:00:00" "2023-10-18 16:00:00")
  ;; => {:provider-id 1,
  ;;     :availablity-day-1
  ;;     {:start-time "2023-10-17 08:00:00",
  ;;      :end-time "2023-10-17 17:00:00"},
  ;;     :availablity-day-2
  ;;     {:start-time "2023-10-18 07:00:00", :end-time "2023-10-18 16:00:00"}}

  (def provider1
    {:provider-id 1
     :availablity-day-1 {:start-time "2023-10-17 08:00:00"
                         :end-time "2023-10-17 17:00:00"}
     :availablity-day-2 {:start-time "2023-10-18 07:00:00"
                         :end-time "2023-10-18 16:00:00"}})

  (build 2 "09:30:00" "18:30:00" "07:45:00" "18:15:00")
  ;; this is simplified to only time submissions
  ;; => {:provider-id 2,
  ;;     :availablity-day-1 {:start-time "09:30:00", :end-time "18:30:00"},
  ;;     :availablity-day-2 {:start-time "07:45:00", :end-time "18:15:00"}}

  (build 3 800 1700 700 1600)
  ;; this is simplified to military time submissions with no 0 in front
  ;; => {:provider-id 3,
  ;;     :availablity-day-1 {:start-time 800, :end-time 1700},
  ;;     :availablity-day-2 {:start-time 700, :end-time 1600}}

)

(comment

; Next step: AVAILABILITY FN

; allow patient to retrieve available appt slots

; how do we turn the above map into 15 min slots?
;  (-> each day's range
;      represented as 15 min slots
;      associated with given provider)

; get availability-day-1 start time. surprise! its a string.
; get availability-day-1 end time. surprise! its a string.
; ? make range : (range start end step) 
; ? use clj-time with datetimes?

(range 800 1700 15)
;; => (800
;;     815
;;     830
;;     845
;;     860
;;    .....
;;     1625
;;     1640
;;     1655
;;     1670
;;     1685)

; Ok, this gives me a range but its not in the format i need... i need 860 = 900

; can i step by a fraction?
; yes, but this logic doesnt understand "60 min in hour"

(range 800 1100 1/4)

; what if we get the difference between end and start...
(- end-epoch start-epoch)
(- 1697576400 1697558400)
;; => 18000 (seconds)
;; then divide by 3600 to get the hours
(/ 18000 3600)
;; => 5 (hours) between end and start

; would we then inc by 15 minutes? no bc it still will give 860 instead of 900 i think

; what if we
; take the start-epoch
; inc by 15 over and over until we reach the end epoch
; convert each result to a time like 8:15

; or can i make range work if i use the epochs and stepping by seconds?....
(range 1697558400 1697576400 (* 60 15))
;; => (1697558400
;;     1697559300
;;     1697560200
;;     1697561100
;;     1697562000
;;     1697562900
;;     1697563800
;;     1697564700
;;     1697565600
;;     1697566500
;;     1697567400
;;     1697568300
;;     1697569200
;;     1697570100
;;     1697571000
;;     1697571900
;;     1697572800
;;     1697573700
;;     1697574600
;;     1697575500)

; I bet i can convert each of these epochs to a time!!!!!!!

; look into tick clj time library

  )
