# funcScript

A simplistic embeddable purely functional scripting language

## Installation/Building

To install/build this, you will need OCaml version 4.12 or later, as well as opam, with the packages menhir and dune installed.

### Installing as an opam package

(Note: no releases have been made yet, for now, build from source)

First, add the repo to opam with the following command:

```
opam repo add KirrimK https://github.com/KirrimK/opam-repo.git
opam update
```

Then, install the package with the following command:

```
opam install funcScript
```


### Building from source

Clone this repo, and in the project root, run ```dune build```. This build the library, the executable (a REPL you can use to test the language), and the tests.
Then run ```dune exec fSrepl``` to run the REPL, or ```dune runtest``` to run the unittests.

To install this as a dev opam package, and be able to import this in your own OCaml projects, run ```opam install .```

## Language

### Values

- None (```#```)
- int
- float
- bool
- string
- list
- function
All objects are first-class.
The type of an object can be checked at runtime with the ```type``` function.

Note: the typing system is still a work in progress at this point.

### Syntax

#### Assigning a value to a variable
```
<var_name>, <var_name>, ... = <var_value>, <var_value>, ... in
...
```
Multiple assignments require having the same number of variables and values.

Individual elements can also be acquired by destructuring lists:
```
[<var_name>, <var_name>, ...] = <something_evaluating_to_a_list> in (array-style)
or
<var_name>::<var_name>, ... = <something_evaluating_to_a_list>, ... in (list-style)
...
```
If there are more variables extracted than there are elements in the list, the excess variables will be assigned the value None.
If there are more elements in the list than variables extracting values, the excess elements will be dropped.
This process can be recursive.

#### Conditional statements
```
if <expression> then <statement> else <statement>
```
Both statements should return the same type of value.

#### Calling a function
```
<function_name>(<arg>, <arg>, ...)
or
<expression_evaluating_to_a_function>(<arg>, <arg>, ...)
```
Partial calling of a function is allowed, and returns a function waiting for the remaining arguments to be called before executing its code.

#### Declaring a function
```
<function_name> = (<arg>, <arg>, ... ->
    <statement>
) in ...
```

#### Declaring a block

A statement can be encased in a block:
```
bg <statement> nd
```

### Operators

The operations carried by operators depend on the type of the operands.
Here are tables detailing the operations for each operator.
X means that the operation is not supported.

#### Operator ```+```
left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |boolean OR|X |X    |X     |X     |X       
Int       |X     |X     |addition|X  |X     |X     |X       
Float     |X     |X     |X     |addition|X  |X     |X       
String    |X     |X     |X     |X    |concatenation|X|X     
List      |X     |X     |X     |X    |X     |concatenation|X
Function  |X     |X     |X     |X    |X     |X     |X       

#### Operator ```-```
left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |X     |X     |X    |X     |X     |X       
Int       |X     |X     |substraction|X|X   |X     |X       
Float     |X     |X     |X     |substraction|X|X   |X       
String    |X     |X     |X     |X    |X     |X     |X       
List      |X     |X     |X     |X    |X     |remove items from left that are present in right|X
Function  |X     |X     |X     |X    |X     |X     |X       

#### Operator ```*```
left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |boolean AND|X|X    |X     |X     |X       
Int       |X     |X     |multiplication|X|X |X     |X       
Float     |X     |X     |X     |multiplication|X|X |X       
String    |X     |X     |repeats string content n times|X|X|X|X
List      |X     |X     |repeats list content n times|X|X|X|X
Function  |X     |X     |X     |X    |X     |X     |X       

#### Operator ```/```
left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |X     |X     |X    |X     |X     |X       
Int       |X     |X     |floor division|X|X |X     |X       
Float     |X     |X     |X     |division|X  |X     |X       
String    |X     |X     |X     |X    |X     |X     |X       
List      |X     |X     |X     |X    |X     |X     |X       
Function  |X     |X     |X     |X    |X     |X     |X       

#### Operator ```%```
left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |X     |X     |X    |X     |X     |X       
Int       |X     |X     |modulo|X    |X     |X     |X       
Float     |X     |X     |X     |modulo|X    |X     |X       
String    |X     |X     |X     |X    |X     |X     |X       
List      |X     |X     |X     |X    |X     |X     |X       
Function  |X     |X     |X     |X    |X     |X     |X       

#### Operator ```::```

Used to add an element to the beginning of a list.
Accepted parameters:
    - left: element of type 'a
    - right: list containing elements of type 'a

#### Operator ```==```
Tests the strict equality between two objects (objects must have the same type and same values to be equal).

#### Operator ```!=```
Tests the inequality between two objects.

#### Operator ```<```

left/right|None  |Bool  |Int   |Float|String|List  |Function
----------|------|------|------|-----|------|------|--------
None      |X     |X     |X     |X    |X     |X     |X        
Bool      |X     |X     |X     |X    |X     |X     |X       
Int       |X     |X     |<     |X    |X     |X     |X       
Float     |X     |X     |X     |<    |X     |X     |X       
String    |X     |X     |X     |X    |X     |X     |X       
List      |X     |X     |X     |X    |X     |X     |X       
Function  |X     |X     |X     |X    |X     |X     |X  

#### Operator ```>```
Same behaviour as ```<```

#### Operator ```<=```
Same behaviour as ```<```

#### Operator ```>=```
Same behaviour as ```<```

#### Operator ```!```

This operator is unary.

operand   |Result
----------|------
None      |X
Bool      |boolean NOT
Int       |X
Float     |X
String    |X
List      |X
Function  |X