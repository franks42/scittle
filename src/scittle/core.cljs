(ns scittle.core
  (:refer-clojure :exclude [time])
  (:require [cljs.reader :refer [read-string]]
            [goog.object :as gobject]
            [goog.string :as gstring]
            [goog.string.format]
            [sci.core :as sci]
            [sci.ctx-store :as store]
            [sci.impl.unrestrict]
            [scittle.impl.common :refer [cljns]]
            [scittle.impl.error :as error]
            [clojure.string :as str]))

(set! sci.impl.unrestrict/*unrestricted* true)

;; make document conditional
(def ^js doc js/globalThis.document)

(clojure.core/defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (cljs.core/system-time)
         ret# ~expr]
     (prn (cljs.core/str "Elapsed time: "
                         (.toFixed (- (system-time) start#) 6)
                         " msecs"))
     ret#))

(def stns (sci/create-ns 'sci.script-tag nil))
(def rns (sci/create-ns 'cljs.reader nil))

(def namespaces
  {'clojure.core
   {'time (sci/copy-var time cljns)
    'system-time (sci/copy-var system-time cljns)
    'random-uuid (sci/copy-var random-uuid cljns)
    'read-string (sci/copy-var read-string cljns)
    'update-keys (sci/copy-var update-keys cljns)
    'update-vals (sci/copy-var update-vals cljns)
    'parse-boolean (sci/copy-var parse-boolean cljns)
    'parse-double (sci/copy-var parse-double cljns)
    'parse-long (sci/copy-var parse-long cljns)
    'parse-uuid (sci/copy-var parse-uuid cljns)
    'NaN? (sci/copy-var NaN? cljns)
    'infinite? (sci/copy-var infinite? cljns)
    'iteration (sci/copy-var iteration cljns)
    'abs (sci/copy-var abs cljns)
    'regexp? (sci/copy-var regexp? cljns)
    'iterable? (sci/copy-var iterable? cljns)
    'cloneable? (sci/copy-var cloneable? cljns)
    'reduceable? (sci/copy-var reduceable? cljns)
    'js-symbol? (sci/copy-var js-symbol? cljns)
    'demunge (sci/copy-var demunge cljns)
    'swap-vals! (sci/copy-var swap-vals! cljns)
    'reset-vals! (sci/copy-var reset-vals! cljns)
    'add-tap (sci/copy-var add-tap cljns)
    'remove-tap (sci/copy-var remove-tap cljns)
    'tap> (sci/copy-var tap> cljns)
    'Cons cljs.core/Cons
    'Atom cljs.core/Atom
    'Keyword cljs.core/Keyword
    'Symbol cljs.core/Symbol
    'UUID cljs.core/UUID
    'MapEntry cljs.core/MapEntry
    'PersistentVector cljs.core/PersistentVector
    'PersistentQueue (let [x cljs.core/PersistentQueue]
                       (gobject/set x "EMPTY" cljs.core/PersistentQueue.EMPTY)
                       x)
    'List cljs.core/List
    'PersistentHashMap cljs.core/PersistentHashMap
    'PersistentHashSet cljs.core/PersistentHashSet
    'PersistentArrayMap cljs.core/PersistentArrayMap
    'PersistentTreeMap cljs.core/PersistentTreeMap
    'PersistentTreeSet cljs.core/PersistentTreeSet
    'Volatile cljs.core/Volatile
    'Repeat cljs.core/Repeat
    'Cycle cljs.core/Cycle
    'Iterate cljs.core/Iterate
    'IntegerRange cljs.core/IntegerRange
    'Range cljs.core/Range
    'IndexedSeq cljs.core/IndexedSeq
    'Subvec cljs.core/Subvec
    'ChunkedSeq cljs.core/ChunkedSeq
    'ChunkedCons cljs.core/ChunkedCons
    'TaggedLiteral cljs.core/TaggedLiteral
    'ExceptionInfo cljs.core/ExceptionInfo
    'Reduced cljs.core/Reduced
    'Throwable->map (sci/copy-var Throwable->map cljns)
    'js-invoke (sci/copy-var js-invoke cljns)
    'js-mod (sci/copy-var js-mod cljns)}
   'goog.object {'set gobject/set
                 'get gobject/get}
   'goog.string {'format gstring/format
                 'htmlEscape gstring/htmlEscape}
   'goog.string.format {} ;; For cljs compatibility
   'sci.core {'stacktrace sci/stacktrace
              'format-stacktrace sci/format-stacktrace}})

(defn load-fn [{:keys [ctx] :as opts}]
  (when-let [lib (and (string? (:namespace opts))
                      (gobject/get js/globalThis (:namespace opts)))]
    (sci/add-js-lib! ctx (:namespace opts) lib)))

(store/reset-ctx!
  (sci/init {:namespaces namespaces
             :classes {'js js/globalThis
                       :allow :all
                       'Math js/Math}
             :ns-aliases {'clojure.pprint 'cljs.pprint}
             :features #{:scittle :cljs}
             :load-fn load-fn}))

(unchecked-set js/globalThis "import" (js/eval "(x) => import(x)"))

(def !last-ns (volatile! @sci/ns))

(defn- -eval-string [s]
  (sci/binding [sci/ns @!last-ns]
    (let [rdr (sci/reader s)]
      (loop [res nil]
        (let [form (sci/parse-next (store/get-ctx) rdr)]
          (if (= :sci.core/eof form)
            (do
              (vreset! !last-ns @sci/ns)
              res)
            (recur (sci/eval-form (store/get-ctx) form))))))))

(defn ^:export eval-string [s]
  (try (-eval-string s)
       (catch :default e
         (error/error-handler e (:src (store/get-ctx)))
         (throw e))))

(defn register-plugin! [_plug-in-name sci-opts]
  (store/swap-ctx! sci/merge-opts sci-opts))

(defn- eval-script-tags* [script-tags]
  (when-let [tag (first script-tags)]
    (if-let [src (.getAttribute tag "src")]
      (let [req (js/XMLHttpRequest.)
            _ (.open req "GET" src true)
            _ (gobject/set req "onload"
                           (fn [] (this-as this
                                    (let [response (gobject/get this "response")]
                                      (gobject/set tag "scittle_id" src)
                                      ;; save source for error messages
                                      (store/swap-ctx! assoc-in [:src src] response)
                                      (sci/binding [sci/file src]
                                        (eval-string response)))
                                    (eval-script-tags* (rest script-tags)))))]
        (.send req))
      (if-let [text (not-empty (str/trim (gobject/get tag "textContent")))]
        (let [scittle-id (str (gensym "scittle-tag-"))]
          (gobject/set tag "scittle_id" scittle-id)
          (store/swap-ctx! assoc-in [:src scittle-id] text)
          (sci/binding [sci/file scittle-id]
            (eval-string text))
          (eval-script-tags* (rest script-tags)))
        (eval-script-tags* (rest script-tags))))))

(defn ^:export eval-script-tags [& [script-tags]]
  (let [script-tags (or script-tags
                        (.querySelectorAll
                          doc "script[type='application/x-scittle']"))
        script-tags (if (or (coll? script-tags)
                            (aget script-tags "length"))
                      script-tags
                      [script-tags])]
    (eval-script-tags* script-tags)))

(def auto-load-disabled? (volatile! false))

(defn ^:export disable-auto-eval
  "By default, scittle evaluates script nodes on the DOMContentLoaded
  event using the eval-script-tags function. This function disables
  that behavior."
  []
  (vreset! auto-load-disabled? true))

(when doc
  (.addEventListener doc
   "DOMContentLoaded"
   (fn [] (when-not @auto-load-disabled? (eval-script-tags))), false))

(enable-console-print!)
(sci/alter-var-root sci/print-fn (constantly *print-fn*))
