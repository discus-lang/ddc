
Primitives
==========

Booleans
--------

.. code-block:: none

 (true  value)     #true                      
 (false value)     #false                     
 (branching)       #if                        


.. code-block:: none

                  e1 ⇓ #true   e2 ⇓ v2                          e1 ⇓ #false  e3 ⇓ v3
 (eval-if-true)  ——————————————————————       (eval-if-false)  ———————————————————————
                  #if e1 e2 e3 ⇓ v2                              #if e1 e2 e3 ⇓ v3


Natural Numbers
---------------

.. code-block:: none

 (value)           #nat-????
 (arithmetic)      #nat-{add,sub,mul,div} 
 (comparison)      #nat-{eq,neq,lt,le,gt,ge}


Lists
-----

.. code-block:: none

 (empty list)      #list-nil
 (cons cell)       #list-cons
 (destructor)      #list-case


.. code-block:: none

                    eScrut ⇓ #list-nil           eAltNil ⇓ v
 (eval-list-case)  ———————————————————————————————————————————
                    #list-case eScrut eAltNil eAltCons ⇓ v

                    eScrut ⇓ #list-cons eH eT    eAltCons eH eT ⇓ v
 (eval-list-case)  —————————————————————————————————————————————————
                    #list-case eScrut eAltNil eAltCons ⇓ v


Matching
--------

.. code-block:: none

 (matching)        #match
 (wildcard)        #o

