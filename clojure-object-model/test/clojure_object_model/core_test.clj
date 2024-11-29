(ns clojure-object-model.core-test
  (:require [clojure.test :refer :all]
            [clojure-object-model.core :refer :all]))

;; ============================
;; Тесты для определения классов
;; ============================
(deftest test-class-definition
  (testing "Class definition and inheritance"
    ;; Проверка определения класса и атрибутов
    (defclass TestClass
      :slots {:id nil :value 0}
      :inherits [])
    (is (= (:slots TestClass) {:id nil :value 0}))  ;; Проверяем слоты
    (is (= (:parents TestClass) []))               ;; Проверяем родителей
    (is (= (:name TestClass) 'TestClass))))        ;; Проверяем имя класса

;; ============================
;; Тесты для контроля доступа к слотам
;; ============================
(deftest test-access-control
  (testing "Access control for slots"
    ;; Определение классов
    (defclass TestClass
      :slots {:id nil :value 0}
      :access {:id :private :value :protected})
    (defclass PublicClass
      :slots {:public-slot 1})

    ;; Экземпляры классов
    (let [instance {:class TestClass :slots {:id 1 :value 42}}
          public-instance {:class PublicClass :slots {:public-slot 1}}]

      ;; Проверка приватного слота
      (is (thrown? Exception (get-slot PublicClass instance :id)))  ;; Доступ запрещён

      ;; Проверка защищённого слота
      (is (thrown? Exception (get-slot PublicClass instance :value)))  ;; Доступ запрещён

      ;; Проверка публичного слота
      (is (= (get-slot PublicClass public-instance :public-slot) 1)))))  ;; Доступ разрешён)))

;; ============================
;; Тесты для set-slot
;; ============================ 
(deftest test-set-slot 
  (testing "Setting slot values with access control"
    ;; Определяем класс 
    (defclass TestClass 
      :slots {:id nil :value 0} 
      :access {:id :private :value :protected :public-slot :public})

    ;; Создаём экземпляр 
    (let [instance {:class TestClass :slots {:id 1 :value 42 :public-slot "test"}}]

      ;; Успешное обновление публичного слота 
      (is (= (set-slot TestClass instance :public-slot "updated") 
             {:class TestClass :slots {:id 1 :value 42 :public-slot "updated"}}))

      ;; Попытка обновления защищённого слота из неродственного класса 
      (is (thrown? Exception (set-slot PublicClass instance :value 100)))

      ;; Попытка обновления приватного слота 
      (is (thrown? Exception (set-slot PublicClass instance :id 2))))))

;; ============================
;; Тесты для множественного наследования
;; ============================ 
(deftest test-linearize 
  (testing "Linearize inheritance hierarchy"
    ;; Определяем классы 
    (def Animal {:name "Animal" :parents [] :slots {}}) 
    (def Mammal {:name "Mammal" :parents [Animal] :slots {}}) 
    (def Dog {:name "Dog" :parents [Mammal] :slots {:breed "unknown"}})
    
    (def Plant {:name "Plant" :parents [] :slots {}}) 
    (def GreenPlant {:name "GreenPlant" :parents [Plant] :slots {}}) 
    (def Hybrid {:name "Hybrid" :parents [Mammal Plant] :slots {:type "unknown"}})

    ;; Проверка простой иерархии: Dog -> Mammal -> Animal 
    (let [linearized-dog (linearize Dog) 
          dog-names (map :name linearized-dog)] 
      (is (= dog-names ["Dog" "Mammal" "Animal"])))

    ;; Проверка множественного наследования: Hybrid -> Mammal, Plant -> Animal 
    (let [linearized-hybrid (linearize Hybrid) 
          hybrid-names (map :name linearized-hybrid)] 
      (is (= hybrid-names ["Hybrid" "Mammal" "Plant" "Animal"])))))

;; ============================
;; Тесты для комбинирования методов
;; ============================ 
(deftest test-method-combination 
  (testing "Method combination using before, after, and primary methods"
    ;; Определение метода с комбинированием 
    (defmethod-combine bark 
      :before (fn [_ _] (println "Preparing to bark")) 
      :primary (fn [_ _] "Woof!") 
      :after (fn [_ _] (println "Bark complete")))

    ;; Проверка основного метода 
    (is (= (bark :primary nil) "Woof!"))  ;; Проверяем основной метод

    ;; Проверка выполнения before-метода 
    (with-out-str 
      (bark :before nil) 
      (is (.contains (with-out-str (bark :before nil)) "Preparing to bark")))

    ;; Проверка выполнения after-метода 
    (with-out-str 
      (bark :after nil) 
      (is (.contains (with-out-str (bark :after nil)) "Bark complete")))))

;; ============================
;; Тесты для диспетчеризации методов
;; ============================ 
(deftest test-methods 
  (testing "Method dispatch and inheritance"
    ;; Определение классов 
    (defclass Parent :slots {:id nil}) 
    (defclass Child :inherits [Parent])

    ;; Определение методов 
    (defmethod call-method [Parent :identify] [_ _] "Parent method") 
    (defmethod call-method [Child :identify] [_ _] "Child method")

    ;; Проверка методов для Parent и Child 
    (is (= (call-method :identify {:class Child}) "Child method"))  ;; Метод из Child 
    (is (= (call-method :identify {:class Parent}) "Parent method"))))  ;; Метод из Parent

;; Запуск всех тестов
  (run-tests)
