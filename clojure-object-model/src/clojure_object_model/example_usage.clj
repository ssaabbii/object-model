(ns clojure-object-model.example-usage
  (:require [clojure-object-model.core :refer :all]))

;; ============================
;; Классы: определяем наследование и слоты
;; ============================

;; Базовый класс Animal с публичным и защищённым доступом к слотам
(defclass Animal
  :slots {:name "unknown" :age 0}
  :access {:name :public :age :protected})

;; Класс Dog наследуется от Animal
(defclass Dog
  :inherits [Animal]
  :slots {:breed "unknown"}
  :access {:breed :protected})

;; ============================
;; Экземпляры: создаём объект Dog
;; ============================

(def my-dog
  {:class Dog
   :slots {:name "Rex"         ;; Публичный слот
           :age 3              ;; Защищённый слот
           :breed "Labrador"}}) ;; Защищённый слот

;; ============================
;; Методы: доступ к слотам и их обработка
;; ============================

;; Метод для доступа к защищённому слоту breed из класса Dog
(defmethod call-method [Dog :get-breed] [_ instance]
  (get-slot Dog instance :breed))

;; Метод для описания объекта Dog
(defmethod call-method [Dog :describe] [_ instance]
  (str "A " (get-slot Dog instance :breed) " dog named " (get-slot Dog instance :name)))



;; ============================
;; Ромбовидная линеаризация
;; ============================

;; Базовый класс A
(defclass A
  :slots {:base-value 0}
  :access {:base-value :public})

;; Класс B, наследуется от A
(defclass B
  :inherits [A]
  :slots {:b-value 10}
  :access {:b-value :public})

;; Класс C, также наследуется от A
(defclass C
  :inherits [A]
  :slots {:c-value 20}
  :access {:c-value :public})

;; Класс D с множественным наследованием от B и C
(defclass D
  :inherits [B C]
  :slots {:d-value 30}
  :access {:d-value :public})

;; Методы для демонстрации цепочки вызовов

;; Метод в классе A
(defmethod call-method [A :process] [method instance & args]
  (println "Метод process в классе A")
  (get-slot A instance :base-value))

;; Метод в классе B
(defmethod call-method [B :process] [method instance & args]
  (println "Метод process в классе B")
  (let [base-result (call-method :process (assoc instance :class A))]
    (+ base-result (get-slot B instance :b-value))))

;; Метод в классе C
(defmethod call-method [C :process] [method instance & args]
  (println "Метод process в классе C")
  (let [base-result (call-method :process (assoc instance :class A))]
    (+ base-result (get-slot C instance :c-value))))

;; Метод в классе D
(defmethod call-method [D :process] [method instance & args]
  (println "Метод process в классе D")
  (let [b-result (call-method :process (assoc instance :class B))
        c-result (call-method :process (assoc instance :class C))]
    (+ b-result c-result (get-slot D instance :d-value))))

;; Создаем экземпляр класса D
(def my-d-instance
  {:class D
   :slots {:base-value 1
           :b-value 11
           :c-value 21
           :d-value 31}})

(defn demonstrate-diamond-linearization []
  (println "\n=== Линеаризация классов ===")
  (println "Линеаризация A:" (linearize A))
  (println "Линеаризация B:" (linearize B))
  (println "Линеаризация C:" (linearize C))
  (println "Линеаризация D:" (linearize D))

  (println "\n=== Цепочка вызовов методов ===")
  (let [base-value (get-slot A my-d-instance :base-value)
        b-value (get-slot B my-d-instance :b-value)
        c-value (get-slot C my-d-instance :c-value)
        d-value (get-slot D my-d-instance :d-value)]
    (println "Промежуточные значения:")
    (println "  base-value (A):" base-value)
    (println "  b-value (B):   " b-value)
    (println "  c-value (C):   " c-value)
    (println "  d-value (D):   " d-value)

    (let [result (call-method :process my-d-instance)]
      (println "\nФинальный результат:" result)
      (println "Проверка вычисления: "
               base-value " + "
               b-value " + "
               c-value " + "
               d-value " = "
               result))))


;; ============================
;; Основной пример использования
;; ============================

(defn -main []
  ;; Публичный доступ к имени
  (println "Dog name:" (get-slot Dog my-dog :name))  ;; Ожидается: "Rex"

  ;; Доступ к защищённому слоту age из класса Dog
  (println "Dog age:"
           (try
             (get-slot Dog my-dog :age)  ;; Ожидается: 3
             (catch Exception e (.getMessage e))))  ;; Исключение не должно возникнуть

  ;; Попытка доступа к защищённому слоту age из другого класса (ошибка)
  (println "Attempting access to protected age slot from Animal:"
           (try
             (get-slot Animal my-dog :age) ;; Ошибка: доступ запрещён
             (catch Exception e (.getMessage e))))  ;; Ожидается: "Access denied to protected slot: age"

  ;; Доступ к защищённому слоту breed через метод класса Dog
  (println "Breed (via method):" (call-method :get-breed my-dog)) ;; Ожидается: "Labrador"

  ;; Описание объекта через полиморфизм
  (println (call-method :describe my-dog))  ;; Ожидается: "A Labrador dog named Rex"

  ;; Обновление публичного слота name
  (println "Updating dog's name...")
  (let [updated-dog (set-slot Dog my-dog :name "Buddy")] ;; Обновляем имя
    (println "Updated Dog name:" (get-slot Dog updated-dog :name))) ;; Ожидается: "Buddy"

  ;; Демонстрация ромбовидной линеаризации
  (demonstrate-diamond-linearization))
