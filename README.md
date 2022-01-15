# LIPS-USB

## Phase 4 ADD-ONS

### Functions

In this phase 4 we added support for user defined functions using the following syntax: `<return Type> <function Name> (args) {<function body>}`, where `args = <argument Type> <argument Name>`. Thus a simple function can be defined as:


```
>>> int succ(int n) {return n+1}
ACK: int succ (int n) {
return n + 1
}

>>> int succTimesTwo(int n){int auxVar := 2; return succ(n)*auxVar}
ACK: int succTimesTwo (int n) {
int auxVar := 2;
return succ(n) * auxVar
}

>>> succTimesTwo(2)
OK: succTimesTwo(2) ==> 6
```


Functions can also be declared inside other functions:

```
>>> int closure(int p){int adder(int x, int p){return x+p}; return adder(p,2) }
ACK: int closure (int p) {
int adder (int x,int p) {
return x + p
}
;
return adder(p,2)
}

>>> closure(10)
OK: closure(10) ==> 12
```

If a function needs a value that's not currently declared in the present scope, it shall try to look it up
recursively in the scopes above it:

```
>>> int closure2(int p){int innerOne(){return p *2}; return innerOne()+1}
ACK: int closure2 (int p) {
int innerOne () {
return p * 2
}
;
return innerOne() + 1
}

>>> closure2(5)
OK: closure2(5) ==> 11
```

Functions are also capable of recursion, some recursion examples are:


```
>>> int factorial(int n){return if(n = 0, 1,n * factorial(n-1))}
ACK: int factorial (int n) {
return if(n = 0,1,n * factorial(n - 1))
}

>>> factorial(5)
OK: factorial(5) ==> 120

>>> int fibonacci(int n){return if(n <= 0, 1, fibonacci(n-1) + fibonacci(n-2))} 
ACK: int fibonacci (int n) {
return if(n <= 0,1,fibonacci(n - 1) + fibonacci(n - 2))
}

>>> fibonacci(5)
OK: fibonacci(5) ==> 13
```

### Auto Casting

Expressions are auto casted by default, so the following expressions are all valid:

```
>>> 3 + true       
OK: 3 + True ==> 4
>>> 3 || 0
OK: 3 || 0 ==> True
>>> 3 && 0 
OK: 3 && 0 ==> False
>>>
```

If autocasting is undesaribable, one can turned it off by using the special command: `.acOFF`:

```
>>> 3 + true  .acOFF
Auto Cast is OFF!
>>> 3 + true  
Bad input types!
>>> 3 || 0
Bad input types!
>>> 3 && 0
Bad input types!
```

In order to enable it again, one can do it by using the special command: `ac.ON`.

## Regenerate Expressions

Expressions are regenerated as seen in class:

```
>>> 1 + 2 + 3
OK: (1 + 2) + 3 ==> 6
>>> 1 + 2 - 3
OK: (1 + 2) - 3 ==> 0
>>> 1 + 2 + 3 ^ 2 ^ 3
OK: (1 + 2) + 3 ^ (2 ^ 3) ==> 6564
>>> 1 + (2+3)
OK: 1 + (2 + 3) ==> 6
>>> 1 + (2+3) + (3^2)^3
OK: (1 + (2 + 3)) + (3 ^ 2) ^ 3 ==> 735
```

## New functions

Two new functions were added: `logB2` and `toBinary`, which gets the logarithm in base 2 of a number, and transforms a number to binary respectively:


```
>>> logB2(15)
OK: logB2(15) ==> 3
>>> logB2(10 + 5) 
OK: logB2(10 + 5) ==> 3
>>> toBinary(1)
OK: toBinary(1) ==> 1
>>> toBinary(2)
OK: toBinary(2) ==> 10
>>> toBinary(3)
OK: toBinary(3) ==> 11
>>> toBinary(4)
OK: toBinary(4) ==> 100
>>> toBinary(5)
OK: toBinary(5) ==> 101
>>> toBinary(6)
OK: toBinary(6) ==> 110
>>> toBinary(-1)
OK: toBinary(-1) ==> -1
>>> toBinary(-2) 
OK: toBinary(-2) ==> -10
>>> toBinary(-3) 
OK: toBinary(-3) ==> -11
```
`toBinary` of any negative number yields it's representation in Signed Magnitude (with a sign at the beginning).

## Bugs 

### Bugs about Functions

Sadly, although we tried to propagate returns out of functions, we couldn't manage to do it, so 
the following definitions shall be syntactically correct, but it will yield a type error when it is
evaluated:

```
>>> int badFac(int n){ if (n <= 0, 1, return n* badFac(n-1))}  
ACK: int badFac (int n) {
if(n <= 0,1,return n * badFac(n - 1))
}

>>> badFac(3)
Error: la expresion return(*(Var n,FApp(badFac,-(Var n,1)))) NO es de tipo aritmetico
```

A solution to this would be promoting the `if` function to a expression, and thus we would not have the headache of
trying to handle returns inside functions.

Cvalues/types/ltypes of functions that are applied does not yield errors, instead, they return the cvalue/type/ltype of the function:

```
>>> int id(int n){return n}
ACK: int id (int n) {
return n
}
>>> cvalue(id(5))
OK: cvalue(id(5)) ==> return n
```


## Installing and Running the REPL

It's enough to do: `stack build` to build the project and then `stack run` to run the REPL.

To quit the REPL we added an additional command: `.q`

If you want to check the project on the browser, you can open the docs running: `stack haddock --open .` The documentation will look like:

<center>

![main page](/Imgs/haddock1.PNG)
![main page](/Imgs/haddock2.PNG)

</center>

If the `#source` button is clicked, then it will show you the code for the module:

<center>

![main page](/Imgs/haddock3.PNG)

</center>

