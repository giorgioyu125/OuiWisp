### What is Symploke?

Symploke is a simple and logic-focused implementation of Lisp with a Generational GC.

## What is Lisp? And why i have chose it? 
##       (A general Overview)

Lisp is a multi-paradigm interpreted programming language focused on functional and pure 
programming,it does have a Object System, but this not mean is a Object-Oriented Programming 
language in the usual way we are used to think. Classes are more like types but they appear 
in a symbolic table and a virtual machine. The virtual machine is a necessary component of 
the Symploke Interpreter, because we are implementeing a complex memory model that involeves 
mainly four different components of the VM: Garbage Collector, Abstract Syntax Tree, Evaluator 
and Symbolic Table (The semantic core of our language).


## Phases of Symploke Interpreter Execution

### The Lexer, the first part:

The lexer is the set of procedures or functions that is made to transform the source code text into a list of ordered idioms 
defined by the language, and later this will be used by the syntax analyzer, semantic checker and the symbol table to execute,
check and collect informations.

In Symploke the types are optional because as the name suggest we dont look too much at the "Intrinsic Property" but at the 
relation or the form of this relations. But in "some" sense every atom is simply a collection of letters, this does not means 
that letters are the fundematal type, just that every atom is made of them.

The Lexer transforms every token by recognizing its lenght by reading it and matching it in its internal strucuture.
Like in this example:

( + 1 2 )

Lexer output:

LEFT_PAREN ( 1 1 1
PLUS_OP + 1 1 1
INT 1 1 1
...
(LeftParen, "(", 1, 0, 1), (PlusOp, "+", 1, 1, 1), (Int, "1", 1, 1, 1), (Int, "2", 1, 1, 1), (RightParen, ")", 1, 0, 1)
or
(kind lexeme span depth sexpr_id)


The first 4 parameters of a token are just immediate and can be calculated without any problem, but. The 
last one needs some more attention. Because the number of sexpr_id is the number of open parenthesis and assign to each 
the correct is id is not immediate. Because the same "depth" of innesting can corrispond to different s-exp we need 
a fancier algorithm to express correctly this last paramater.

The algorithm is indeed *very simple* and it looks like this little rules:

Create a stack a empty stack. and manipulate this stack for every token seen left to right.

Push - a new id when you encounter a open praenthesis '('
Pop - when you find a closed parenthesis pop from the stack the upmost element.
Peek&Assign - when you find an atom peek the stack upmost id and assign it to that token.


### The AST constructor, the second part:

This part is concerned about creating the AST accordingly to the syntax rules of the language. Note that the AST 
construction itself is the syntax checking, so if the AST cannot be constructed properly the syntax of the program
is wrong.

After this, lets showcase the AST constructor algorithm: 

    Take the following exp as an example:

    (+ (+ 1 2) (+ 7 8))

    Steps: 

    1. Tokenize the input into a list of tokens (atoms, parentheses).
    Use a recursive descent parser:
    2. When you see (, recursively parse the subexpression until you see ).
    3. Build the AST as a tree where each node has:
        A value (like +, 1, etc.)
        A list of children (subexpressions).
    4. Return the root node of the expression.

    Expected output: 
    (+ ( ( )
       | |
       + +
       | |
       1 7
       | |
       2 8
       | |
       ) )







### Symbolic Table

We are not yet discussing what is a *Symbol* in Lisp and Symploke formally but we can says that a symbol is:
"The reference to an object", thats right, very simple and straightforawrd. So the symbol have a pair in memory that is the 
pointer of that object in the Pile. This pointer description is present in an actual database that we will implement as HashTable,
and the Key being the symbol and the Value the description of that object connected to the symbol and the location in the stack of 
the symbol. So we give access to creating a new object using the quote notation used down below:

```symploke
(quote (1 (+ 2 3)))
```

Other than this we can say that the value of a symbol is an object and is essentialy an array 
that lists all its property in order. Because every object needs to fill the same fields we use an array
with size known at compile time but a number of them (number of symbol)only know at runtime or when 
evaluating the programs expressions. This is a example table for our program:

| Name | TypeTag | Flags (bitset) | Arity (packed) | ValuePtr | EntryPtr | EnvID  | StackSlot |      GCInfo (packed)     |      SizeBytes       |
|:----:|:-------:|:--------------:|:--------------:|:--------:|:--------:|:------:|:---------:|:------------------------:|:--------------------:|
|  x   |   var   | (1 0 ...... 0) |      NIL       |   &heap  |   NIL    |   1    |     0     | (gen,&age-brigade,color) |    ???               |
|  5   |   int   | (1 0 ...... 0) |      NIL       |   &heap  |   NIL    |   1    |     1     | (gen,&age-brigade,color) |     4                |
|"Save"|   str   | (1 0 ...... 0) |      NIL       |   &heap  |   NIL    |   1    |     2     | (gen,&age-brigade,color) |     4                |
|(! 3) |  s-exp  | (1 0 ...... 0) |      NIL       |   &heap  |   NIL    |   2    |     3     | (gen,&age-brigade,color) |    ???               |
| car  |  prim-f | (1 0 ...... 0) |       1        |   &car   | &native  |   2    |     4     | (gen,&age-brigade,color) | (car functions size) |


TypeTag: what this is (int, float, cons, string, symbol, function, macro, special-form, vector, nil, bool…).

EvalKind: how it participates in evaluation (data | function | macro | special-form). Lets the evaluator know when 
to evaluate arguments and how.

Flags (bitset): common traits. Suggested bits: immutable, pure, primitive, variadic, pinned, external, const-binding.

Arity (packed): for callables. Pack min/max arity into one 32-bit value (low16=min, high16=max; 0xFFFF means “unbounded”). 
For data, set to 0.

ValuePtr: pointer to the payload (heap object) or an immediate/NaN-boxed value if you choose that representation.
For cons/string/vector this points to the heap block; for ints it can be immediate.

EntryPtr: code/definition pointer. For primitives: native entrypoint; for user functions: pointer to AST/bytecode
or compiled closure; otherwise null.

EnvPtr: environment/closure pointer for captured variables; null for data and primitives.

StackSlot: index/ID of the root in your pointer pile (Deque). -1 if not 
rooted here (e.g., temporary or global managed elsewhere).

GCInfo (packed): (gen,&age-brigade,color), each one of the is rapresent the generation and which "age range" each
                 object is part of. The color is only put to open up the table to more expressivity and optimization.
                 Probabily will be another table with only three colum that is devided not in symbols but in age-brigade 
                 buckets so that the GC can easily locate younger and probabily less used object.
                 Or maybe the Symbol table address can be devided in this sort of "Age Range Buckets".


SizeBytes: size of the heap payload for copy GC. 0 for immediates.

Read all the tokens separeted left to right and place on the stack the id of the first when you see the first parenthesis,
after that keep going forward until you encounter another parenthesis, from now and then if it is closed pop an element from the stack 
if its open push it on the stack with a new fresh id. Meanwhile every atom you encounter peek from the stack and give to that 
atom that value.
