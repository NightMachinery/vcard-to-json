(ns NightMachinery.vcard-to-json
  (:gen-class)
  (:require [clojure.data.json :as json]))

(import (ezvcard Ezvcard VCard))
(use 'flatland.ordered.map)

;;;
(defn remove-nils [m]
  (let [f (fn [x]
            (do
              ;; (println "x: " x)
              (cond
                (map? x)
                (do
                 ;; (println "map: " x)
                 (let [kvs (filter
                            (fn [x]
                              (let [v (second x)]
                                (cond
                                  (or
                                   (nil? v)
                                   (and (coll? v) (empty? v))) false
                                  :else true)))
                            x)]
                   (do
                     ;; (println "kvs: " kvs)
                     (if (empty? kvs) nil (into {} kvs)))))
                (coll? x)
                (do
                  ;; (println "coll: " x)
                  (when (some some? x)
                    x))
                :else
                (do
                  ;; (println "else: " x)
                  x))))]
    (->>
     (clojure.walk/postwalk f m)
     (filter some?))))

(comment
  (remove-nils [{"a" [1 2 nil 3] "b" [nil nil] "c" [] "d" 9}])

  (clojure.walk/postwalk #(do (println % 1) % 1)
                         {"a" nil
                          "b"
                          {"c" nil}})
  )
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

(def vcard_str "BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//macOS 11.2.1//EN
N:;Peter Black;;;
FN:Peter Black
TEL;type=CELL;type=VOICE;type=pref:+98 413 999 4697
EMAIL;type=INTERNET;type=HOME;type=pref:fake_email@live.com
item1.ADR;type=HOME;type=pref:;;خیابان کوشکی، پلاک ۳;;;;Iran
item1.X-ABADR:ir
item2.X-ABRELATEDNAMES;type=pref:Tatsuya Smith
item2.X-ABLabel:_$!<Friend>!$_
NOTE:some notes here
BDAY:1958-11-14
END:VCARD
BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//macOS 11.2.1//EN
N:;;;;
FN:Apple Inc.
ORG:Apple Inc.;
TEL;type=MAIN;type=pref:1-800-MY-APPLE
item1.ADR;type=WORK;type=pref:;;1 Infinite Loop;Cupertino;CA;95014;United States
item1.X-ABADR:us
item2.URL;type=pref:http://www.apple.com
item2.X-ABLabel:_$!<HomePage>!$_
X-ABShowAs:COMPANY
END:VCARD
BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//macOS 11.2.1//EN
N:;;;;
END:VCARD
")
;;;
(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))
;;;
(defn json-dbg [x]
  (do
    ;; (println "DBG823")
    (println (json/write-str x))
    x))

(defn -main
  "Reads vcards from the stdin and converts them to JSON."
  [& args]
  (let [vcard_str (slurp *in*)]
    (let [parsed (. Ezvcard parse vcard_str)
          contacts (.all parsed)]
      (exfiltrate)

      (->
       (for  [contact contacts
              :let [ ;; _ (println (bean contact))
                    structured-names (.getStructuredNames contact)
                    telephone-numbers (seq (.getTelephoneNumbers contact))
                    notes (.getNotes contact)]]
         ;; https://www.evenx.com/vcard-3-0-format-specification
;;;
         (->
          (do
            (ordered-map
             :names-formatted (map #(.getValue %1)
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
                                   })

             :emails (for [email (.getEmails contact)
                           :let [types
                                 (seq (.getTypes email))]]
                       {:types (map #(.getValue %1) types)
                        :value (.getValue email)
                        })

             :addresses (for [address (.getAddresses contact)
                              :let [types
                                    (seq (.getTypes address))]]
                          {:types (map #(.getValue %1) types)
                           :countries (.getCountries address)
                           :streetAddresses (.getStreetAddresses address)
                           :extendedAddresses (.getExtendedAddresses address)
                           :postalCodes (.getPostalCodes address)
                           })

             :birthdays (for [birthday (.getBirthdays contact)]
                          (.getDate birthday))

             :notes (map #(.getValue %1)
                         notes)

             :other-properties (for [p (.getExtendedProperties contact)]
                         {
                          :parameters
                          (.asMap (.getParameters p))
                          :property-name
                          (.getPropertyName p)
                          :group
                          (.getGroup p)
                          :value
                          (.getValue p)
                          })
             ))
          ;; json-dbg
          ;; (remove-nils)
          ))
       (remove-nils)
       (json/write-str)
       (println)
       )

      (comment
        (def c (nth contacts 0))
        (def contact (nth contacts 0))
        (bean c)
        (filter
         (fn [p]
           (instance? p ezvcard.property.RawProperty))
         (seq (.getProperties c)))
        (.asMap (.getParameters
                 (first (.getExtendedProperties c))))
        (.getGroup (first (.getExtendedProperties c)))
        (.getRelations c)
        (.getAddresses c)
        (.getEmails c)
        (.getDate (.getBirthday c))
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
