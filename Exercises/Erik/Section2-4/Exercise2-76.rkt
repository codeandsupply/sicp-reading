;; Exercise 2.76. As a large system with generic operations evolves,
;; new types of data objects or new operations may be needed. For each
;; of the three strategies -- generic operations with explicit
;; dispatch, data-directed style, and message-passing-style --
;; describe the changes that must be made to a system in order to add
;; new types or new operations. Which organization would be most
;; appropriate for a system in which new types must often be added?
;; Which would be most appropriate for a system in which new
;; operations must often be added?

;; Answer:
;;;;;;;;;;

;; Changes that need to be made:

;; Explicit dispatch:
;;
;; With a new data representation, new versions of all procedures must
;; be written with globally unique names. Additionally, a sentinal
;; function must be written to recognize that construction. Then each
;; public function must be updated with a case statement for the new
;; representation.
;;
;; When a new operation is added, each representation must be updated
;; to include supporting transformations, then a case must be written
;; for each representation in the new operation.

;; Data-directed style:
;;
;; With a new data representation, versions of each public operation
;; must be created and registered with the operation table for this
;; new representation.
;;
;; With a new operation, each representation must be updated to
;; support that operation and register it in the operation table.

;; Message Passing style:
;;
;; With a new data representation, a closure needs to be defined with
;; all supported operations.
;;
;; With a new operation, data objects that support it must be updated
;; to include that operation.

;; Easiest for new data?

;; MP or DD

;; Easiest for new operations?

;; MP or DD
