(ns appt
  (:require [coast]
            [components :refer [container tc link-to table thead tbody td th tr button-to text-muted mr2 dl dd dt submit input label]]))

#_(defn welcome [request]
  [:div "hello world! I'm just a function in the appt ns."])


(defn index [request]
  (let [rows (coast/q '[:select *
                        :from appt
                        :order id
                        :limit 10])]
    (container {:mw 8}

     (when (not (empty? rows))
      (link-to (coast/url-for ::build) "New appt"))

     (when (empty? rows)
      (tc
        (link-to (coast/url-for ::build) "New appt")))

     (when (not (empty? rows))
       (table
        (thead
          (tr
            ))
        (tbody
          (for [row rows]
            (tr
              
              (td
                (link-to (coast/url-for ::view row) "View"))
              (td
                (link-to (coast/url-for ::edit row) "Edit"))
              (td
                (button-to (coast/action-for ::delete row) {:data-confirm "Are you sure?"} "Delete"))))))))))


(defn view [request]
  (let [id (-> request :params :appt-id)
        appt (coast/fetch :appt id)]
    (container {:mw 8}
      (dl
        )
      (mr2
        (link-to (coast/url-for ::index) "List"))
      (mr2
        (link-to (coast/url-for ::edit {::id id}) "Edit"))
      (mr2
        (button-to (coast/action-for ::delete {::id id}) {:data-confirm "Are you sure?"} "Delete")))))


(defn errors [m]
  [:div {:class "bg-red white pa2 mb4 br1"}
   [:h2 {:class "f4 f-subheadline"} "Errors Detected"]
   [:dl
    (for [[k v] m]
      [:div {:class "mb3"}
       (dt (str k))
       (dd v)])]])


(defn build [request]
  (container {:mw 6}
    (when (some? (:errors request))
     (errors (:errors request)))

    (coast/form-for ::create
      

      (link-to (coast/url-for ::index) "Cancel")
      (submit "New appt"))))


(defn create [request]
  (let [[_ errors] (-> (coast/validate (:params request) [[:required []]])
                       (select-keys [])
                       (coast/insert)
                       (coast/rescue))]
    (if (nil? errors)
      (coast/redirect-to ::index)
      (build (merge request errors)))))


(defn edit [request]
  (let [appt (coast/fetch :appt (-> request :params :appt-id))]
    (container {:mw 6}
      (when (some? (:errors request))
        (errors (:errors request)))

      (coast/form-for ::change appt
        

        (link-to (coast/url-for ::index) "Cancel")
        (submit "Update appt")))))


(defn change [request]
  (let [appt (coast/fetch :appt (-> request :params :appt-id))
        [_ errors] (-> (select-keys appt [:appt/id])
                       (merge (:params request))
                       (coast/validate [[:required [:appt/id]]])
                       (select-keys [:appt/id])
                       (coast/update)
                       (coast/rescue))]
    (if (nil? errors)
      (coast/redirect-to ::index)
      (edit (merge request errors)))))


(defn delete [request]
  (let [[_ errors] (-> (coast/fetch :appt (-> request :params :appt-id))
                       (coast/delete)
                       (coast/rescue))]
    (if (nil? errors)
      (coast/redirect-to ::index)
      (-> (coast/redirect-to ::index)
          (coast/flash "Something went wrong!")))))
