# Object-Oriented Model in Clojure

## 📋 Проект

Реализация объектной модели для языка Clojure с поддержкой:
- Определения классов
- Управления атрибутами 
- Контроля доступа
- Множественного наследования
- Динамической диспетчеризации методов

## 🗂 Структура проекта

```
object-model/
├── README.md                   # Текущий документ
└── clojure-object-model/
    ├── README.md               # README подпроекта
    ├── doc/                    # Документация
    │   ├── intro.md
    │   └── plan/
    │       └── README.md       # План разработки
    ├── project.clj             # Конфигурация Leiningen
    ├── src/                    # Исходный код
    │   └── clojure_object_model/
    │       ├── core.clj        # Основная реализация
    │       └── example_usage.clj  # Примеры использования
    └── test/                   # Тесты
        └── clojure_object_model/
            └── core_test.clj   # Модульные тесты
```

## 🚀 Сборка и запуск

### Prerequisites
- Java JDK 
- Leiningen

### Команды

```bash
# Compile the project
lein uberjar

# Run tests
lein test

# Run example usage
lein run -m clojure-object-model.example-usage

# Generate documentation
lein codox
```

## 📚 Документация

### Автоматическая документация
- Расположение: `clojure-object-model/target/doc/index.html`

### План разработки
- Документация 1-го этапа: `doc/plan/README.md`

## 🧪 Примеры использования

Смотрите `src/clojure_object_model/example_usage.clj` 

## 🔍 Требования

### Выполненные требования
- [x] Сборка без IDE (Leiningen)
- [x] Компонентные тесты
- [x] Автоматическая документация
- [x] Модельные примеры

## 📦 Зависимости
- Clojure
- Leiningen

## 📄 Лицензия
Eclipse Public License 2.0

