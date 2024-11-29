(ns clojure-object-model.core
  "Core implementation of a CLOS-like object model in Clojure."
  (:require [clojure.set :as set]))

;; ============================
;; Линеаризация C3: поддержка множественного наследования
;; ============================

(defn linearize [class]
  "Compute the linearization (method resolution order) of a class using C3 linearization.
   - class: текущий класс.
   Возвращает список классов в порядке линеаризации."
  (let [direct-parents (get class :parents [])
        parent-linearizations (map linearize direct-parents)
        candidate-lists (conj parent-linearizations direct-parents [class])]
    (loop [result [] candidates candidate-lists]
      (if (every? empty? candidates)
        result
        (let [eligible (first
                        (filter
                         (fn [c]
                           (every?
                            (fn [other-candidates]
                              (not (some #{c} (rest other-candidates))))
                            candidates))
                         (map first (remove empty? candidates))))]

          (if (nil? eligible)
            (throw (ex-info "Inconsistent hierarchy" {:candidates candidates}))
            (recur
             (conj result eligible)
             (mapv
              (fn [candidate-list]
                (remove #{eligible} candidate-list))
              candidates))))))))

;; ============================
;; Макрос для создания классов
;; ============================

(defmacro defclass [name & options]
  "Defines a class with slots, methods, and inheritance.
   - name: имя класса.
   - options: ключевые параметры, включая :slots, :access, и :inherits."
  (let [opts (apply hash-map options)
        slots (get opts :slots {})
        access (get opts :access {})
        parents (get opts :inherits [])]
    `(def ~name
       {:name '~name
        :slots ~slots
        :access ~access
        :parents ~parents})))

;; ============================
;; Обработка атрибутов с контролем доступа
;; ============================

(defn get-slot [caller-class instance slot-name]
  "Retrieves the value of a slot from an instance with access control.
   - caller-class: класс, вызывающий метод.
   - instance: объект.
   - slot-name: имя слота."
  (let [class (:class instance)
        slot-access (get-in class [:access slot-name] :public)
        hierarchy (linearize class)]
    (cond
      (= slot-access :private)
      (if (= class caller-class)
        (get (:slots instance) slot-name)
        (throw (Exception. (str "Access denied to private slot: " slot-name))))

      (= slot-access :protected)
      (if (some #(= % caller-class) hierarchy)
        (get (:slots instance) slot-name)
        (throw (Exception. (str "Access denied to protected slot: " slot-name))))

      :else
      (get (:slots instance) slot-name))))

(defn set-slot [caller-class instance slot-name value]
  "Sets the value of a slot in an instance with access control.
   - caller-class: класс, вызывающий метод.
   - instance: объект.
   - slot-name: имя слота.
   - value: новое значение."
  (let [class (:class instance)
        slot-access (get-in class [:access slot-name] :public)]
    (cond
      (= slot-access :private)
      (if (= class caller-class)
        (assoc-in instance [:slots slot-name] value)
        (throw (Exception. (str "Access denied to private slot: " slot-name))))

      (= slot-access :protected)
      (if (some #{caller-class} (linearize class))
        (assoc-in instance [:slots slot-name] value)
        (throw (Exception. (str "Access denied to protected slot: " slot-name))))

      :else
      (assoc-in instance [:slots slot-name] value))))

;; ============================
;; Диспетчеризация методов и их комбинирование
;; ============================

(defmulti call-method
  "Dispatches methods based on the class and method name."
  (fn [method instance & _] [(get instance :class) method]))

(defmethod call-method :default [method instance & _]
  (throw (Exception. (str "Method " method " not defined for class " (:class instance)))))

(defmacro defmethod-combine [name & {:keys [before after around primary]}]
  "Combines methods for a class using `before`, `after`, and `around` methods."
  `(do
     (defmulti ~name (fn [method# & args#] method#))
     ~@(when before
         [`(defmethod ~name :before [method# & args#]
             (~before method# args#))])
     ~@(when after
         [`(defmethod ~name :after [method# & args#]
             (~after method# args#))])
     ~@(when around
         [`(defmethod ~name :around [method# & args#]
             (~around method# args#))])
     ~@(when primary
         [`(defmethod ~name :primary [method# & args#]
             (~primary method# args#))])))
