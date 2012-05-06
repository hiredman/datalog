(ns datalog.fj
  (:import (java.util.concurrent ForkJoinPool
                                 ForkJoinTask)))

(set! *warn-on-reflection* true)

(defonce ^ForkJoinPool pool (ForkJoinPool.))

(defn pool-exec [^ForkJoinTask fj]
  (.fork fj))

(defn out-exec [^ForkJoinTask fj]
  (.execute pool fj))

(defmulti join type)

(def ^java.util.concurrent.atomic.AtomicLong join-count
  (java.util.concurrent.atomic.AtomicLong. 0))

(defmethod join ForkJoinTask [^ForkJoinTask fj]
  (.getAndIncrement join-count)
  (.join fj))

(defmethod join :default [o] o)

(def ^:dynamic exec out-exec)

(defmacro fj-task [& body]
  `(ForkJoinTask/adapt
   (reify
     Callable
     (call [_]
       (binding [exec pool-exec]
         ~@body)))))

(defn chain [^ForkJoinTask ft fun]
  (let [fj (fj-task (fun (join ft)))]
    (exec fj)
    fj))

(defn run [fun]
  (let [fj (fj-task (fun))]
    (exec fj)
    fj))

(defmacro fj [& body]
  `(run (fn [] ~@body)))

(defn async-list [& tasks]
  (run #(for [^ForkJoinTask t tasks] (join t))))
