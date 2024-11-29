# clojure-object-model

This project provides an object-oriented model in Clojure, inspired by CLOS (Common Lisp Object System), featuring support for classes, attributes, access control, multiple inheritance, dynamic dispatch, and method combination. 

## Features

- Custom class definition
- Attribute (slot) management
- Fine-grained access control (public, protected, private)
- Multiple inheritance with C3 linearization
- Dynamic method dispatch
- Method combination (before, after, around methods)

## Installation

To build and run the project, you'll need to have [Leiningen](https://leiningen.org/) installed.

Clone the repository and use Leiningen for project management:

```bash
git clone https://github.com/ssaabbii/object-model.git
cd clojure-object-model
```

## Usage

To build the project and generate the documentation, use the following commands:

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

## Documentation

Generated HTML documentation will be available in `target/doc/index.html`
## Options

FIXME: listing of options this app accepts.

## Examples

A comprehensive example of library usage can be found in `example_usage.clj`. 

## Requirements Compliance

| Requirement | Status | Comment |
|------------|--------|---------|
| Class Definition | ✅ | defclass macro supports class creation |
| Attributes/Slots | ✅ | Slots defined with :slots, access control via :access |
| Access Control | ✅ | Implemented :public, :protected, :private |
| Multiple Inheritance | ✅ | Uses C3 linearization algorithm |
| Dynamic Dispatch | ✅ | Implemented via call-method |
| Method Combination | ✅ | Supports before, after, around methods

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2024 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
