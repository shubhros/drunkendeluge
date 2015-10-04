(defn blank? [str]
  (every? #(Character/isWhitespace %) str))

(defn hello-world [username]
  (println (format "Hello, %s" username)))

(defn hello [username]
  (swap! visitors conj username)
  (str "Hello, " username))

