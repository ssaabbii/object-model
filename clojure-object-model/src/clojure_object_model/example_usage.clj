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
    (println "Updated Dog name:" (get-slot Dog updated-dog :name)))) ;; Ожидается: "Buddy"
