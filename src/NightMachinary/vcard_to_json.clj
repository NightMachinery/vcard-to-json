(ns NightMachinary.vcard-to-json
  (:gen-class))

(import (ezvcard Ezvcard VCard))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [vcard_str (slurp *in*)]

    (greet {:name vcard_str})))
