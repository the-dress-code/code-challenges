(ns routes
  (:require [coast]
            [components]))

(def routes
  (coast/routes

    (coast/site
      (coast/with-layout components/layout

        [:get "/" :site.home/index]
        [:get "/welcome" :welcome/welcome]

        [:get "/providers" :provider/build] ; allows provider to submit their schedules (file called provider, fn called build)
        [:get "/patients" :provider/availability] ; allow patient to retrieve available appt slots (file called provider, fn called availability)
	#_[:post "/reservation" :patient/create] ; allow patient to create reservation of 15 min appt slot (fn to create reservation)
	#_[:get "/confirm" :patient/confirm] ; allow patient to confirm reservation (fn to confirm reservation - pop up box?)

        [:resource :appt]))

    (coast/api
      (coast/with-prefix "/api"
        [:get "/" :api.home/index]
       #_ [:get "/appts/welcome" :appt/welcome]
        [:get "/welcome" :welcome/welcome]

        [:resource :appt]))))
