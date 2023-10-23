(ns migrations.20231016213452-create-table-appt
  (:require [coast.db.migrations :refer :all]))

(defn change []
  (create-table :appt
    (datetime :begin)
    (datetime :end)
    (timestamps)))