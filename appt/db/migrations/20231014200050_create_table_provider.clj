(ns migrations.20231014200050-create-table-provider
  (:require [coast.db.migrations :refer :all]))

(defn change []
  (create-table :provider
    (datetime :begin)
    (datetime :end)
    (timestamps)))