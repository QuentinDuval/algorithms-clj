(ns algorithms-clj.rest-router)

(defn user-by-name
  [path-params query-params]
  )

(defn get-all-users
  [path-params query-params]
  )

(def route
  {:path ["user" :name] :get user-by-name})

;; Something like this could be used to make expression template
#_(comp (path "user") (capture :name) (get user-by-name))
#_(comp (path "user") (path "all") (get get-all-users))
#_(comp (path "user") (combine
                        (comp (path "all") (get get-all-users))
                        (comp (capture :name) (get user-by-name))
                        ))