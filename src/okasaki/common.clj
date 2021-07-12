(ns okasaki.common
  (:refer-clojure :exclude [map])
  (:require [okasaki.protocols :as p]
            [clojure.string :as s]
            [clojure.pprint :as pp])
  (:import [clojure.lang Seqable IDeref IRecord ILookup]))

;; fixes lein repl / Cloure 1.10.0 
(prefer-method print-method java.util.Map IDeref)

;; fixes lein repl / Clojure 1.10.1
(prefer-method print-method IRecord IDeref)

;; fixes CIDER / Clojure 1.9.0 / 1.10.0 / 1.10.1
(prefer-method pp/simple-dispatch IRecord IDeref)

(defmacro make-empty
  [nom empty values]
  (let [t (symbol (str "Empty" (name nom)))
        default (if (boolean? empty)
                  (symbol (str "empty-" (s/lower-case (name nom))))
                  empty)]
    `(do (deftype ~t []
           Object
           (equals [this# other#]
             (instance? ~t other#))
           Seqable
           (seq [this#] nil)
           p/Seq
           (->seq [this#] nil)
           p/Empty
           (empty? [this#] true)
           p/Structure
           (->structure [this#] nil)
           IDeref
           (deref [this#] nil)
           ILookup
           (valAt [this# key#] (get ~values key#))
           (valAt [this# key# not-found#] (get ~values key# not-found#)))
         (defmethod print-method ~t [this# w#]
           (print-method nil w#))
         (def ~default (new ~t)))))

(defmacro make-lazy-type
  [nom]
  (let [t (symbol (str "Lazy" (name nom)))]
    `(do (defrecord ~t [~'lazy]
           p/Empty
           (empty? [this#] (p/empty? (deref ~'lazy)))
           p/Seq
           (->seq [this#] (p/->seq (deref ~'lazy)))
           p/Structure
           (->structure [this#] (list :lazy (p/->structure (deref ~'lazy))))
           IDeref
           (deref [this#] (deref ~'lazy)))
         (defmacro ~'$ [~'& r#]
           (list 'new ~t (list* 'delay r#))))))

(defn map->structure
  [m]
  (into {} (clojure.core/map (fn [[k v]] [k (p/->structure v)]) m)))

(defmacro make-type
  {:style/indent [3 :form :form :form [1]]}
  [nom args & [m & rr :as r]]
  (let [opts? (map? m)
        {:keys [make-lazy? empty-name make-empty? empty-values empty?]} (when opts? m)
        r (if opts? rr r)]
    `(do ~(when (or make-empty? empty-name) `(make-empty ~nom ~(or make-empty? empty-name) ~(or empty-values {})))
         ~(when make-lazy? `(make-lazy-type ~nom))
         (defrecord ~nom ~args
           p/Empty
           (empty? [this#] ~(or empty? false))
           p/Structure
           (->structure [this#] [~(keyword (name nom)) (map->structure this#)])
           ~@r)
         (defmethod print-method ~nom [this# ^java.io.Writer w#]
           (.write w# ~(str "->" (name nom) " "))
           (print-method (p/->seq this#) w#)))))

;;

(defn reverse-helper
  [lst reversed]
  (if (p/empty? lst)
    reversed
    (recur (p/tail lst) (p/conj reversed (p/head lst)))))

