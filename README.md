# okasaki

Purely Functional Data Structures in Clojure

https://www.amazon.com/gp/product/0521663504

## Assumptions

- every data structure in separate namespace
- common API as protocols, however all API functions are duplicated in data structure namespaces (to wrap lazy cases)
- every data structure has own type (eg. LeftistHeap)
- if necessary empty is implemented as another type (eg. EmptyLeftistHeap)
- Clojure interop:
    - `->seq` and `seq` -> Clojure sequence
    - `->TYPE` (eg. `->stack`) to convert sequence to desired type
    - `->structure` to export internal structure as map
    - Indexed (for destructuring) and Seqable (for `seq` coersion) interfaces are implemented
- no speed or memory optimizations, possible SO exceptions

### Creating

Two ways:

- `empty-TYPE` (eg. `empty-heap`) singleton is created for every data structure. Use it and feed with data.
- `->TYPE` (eg. `->heap`) to convert Clojure sequence to a type

### Lazy evaluation

This is probably done wrong. I'm using `delay` and `force` to implement `$` operator.

## Other attempts

* https://github.com/jmgimeno/okasaki-clojure
* https://github.com/leonardoborges/purely-functional-data-structures
