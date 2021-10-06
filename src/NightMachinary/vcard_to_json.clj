(ns NightMachinary.vcard-to-json
  (:gen-class))

(import (ezvcard Ezvcard VCard))

;;;
(defmacro locals []
  (into {}
        (map (juxt name identity))
        (keys &env)))

(defn defs [vars]
  (doseq [[k v] vars]
    (eval (list 'def (symbol k) (list 'quote v)))))


(defmacro exfiltrate []
  `(let [ls# (locals)]
     (defs ls#)))

(comment
  (let [a "deusVult"
        b 81]
    (exfiltrate)
    (apply str (concat (str b) a))))
;;;
(def vcard_str "BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//macOS 11.2.1//EN
N:یدالله;دایی;;;
FN:دایی یدالله
TEL;type=HOME;type=VOICE;type=pref:+98 21 3356 3229
TEL;type=WORK;type=VOICE:+98 21 3316 3543
TEL;type=HOME;type=VOICE:+98 21 3356 3249
TEL;type=HOME;type=VOICE:+98 21 3312 0599
TEL:+98 912 102 1217
END:VCARD
BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//macOS 11.2.1//EN
N:یزدی;دکترشوریده;;;
FN:دکترشوریده یزدی
END:VCARD")
;;;
(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [vcard_str (slurp *in*)]
    (let [parsed (. Ezvcard parse vcard_str)
          contacts (.all parsed)]
      (exfiltrate)

      (for  [contact contacts
             :let [ ;; _ (println (bean contact))
                   structured-names (.getStructuredNames contact)
                   telephone-numbers (seq (.getTelephoneNumbers contact))]]

        {:names-formatted (map #(.getValue %1)
                               (.getFormattedNames contact))
         :names-given (map #(.getGiven %1)
                           structured-names)
         :names-family (map #(.getFamily %1)
                            structured-names)
         :telephone-numbers (for [telephone-number telephone-numbers
                                  :let [types
                                        (seq (.getTypes telephone-number))]]
                              {:types (map #(.getValue %1) types)
                               :text (.getText telephone-number)
                               })})
      ;; @todo prune empty elements

      (comment
        (def c (nth contacts 0))
        (bean c)
        (def ts (seq (.getTelephoneNumbers c)))
        (def t (nth ts 0))
        (bean t)
        (.getText t)
        (def types (seq (.getTypes t)))
        (def type0 (nth types 0))
        (bean type0)
        (.getValue type0)

        (def snames (.getStructuredNames c))
        (identity snames)
        (.-given (first snames))
        (.getGiven (first snames))
        (bean (first snames)) (* 7 2)
        (:given (bean (first snames)))))))
