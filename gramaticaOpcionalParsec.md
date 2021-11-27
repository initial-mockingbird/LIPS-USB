# Gramática de LIPS-USB

Tercera Gramatica implementada. Usada por Parsec (top down parser)

## Gramática para la Documentación

### Entrada  

	<entrada> -> <acción> | <exp>

### Acciones

	<acción> -> <definición> | <asignación>

	<definición> -> <Tipo> <asignacion>

	<Tipo> -> lazy <Tipo> | int | bool

	<asignación> -> <id> ':=' <exp>

### Expresiones

	<exp> 
		-> '-' <exp>
		|  '+' <exp>
		|  '!' <exp>
		| <exp> '=' <exp>
		| <exp> '<>' <exp>            
    	| <exp> '+' <exp>             
    	| <exp> '-' <exp>             
    	| <exp> '*' <exp>             
    	| <exp> '^' <exp>             
    	| <exp> '<' <exp>             
    	| <exp> '>' <exp>             
    	| <exp> '<=' <exp>            
    	| <exp> '>=' <exp>            
    	| <exp> '||' <exp>            
    	| <exp> '&&' <exp>            
    	| <id>
    	| '(' <exp> ')'              
    	| ''' <exp> '''              
    	| <FApp>
    	| <Constant>

	<id> -> TkId string
	
	<Constant> -> true | false | integer

	<Fapp> -> <id> '(' <Args> ')'

	<Args> -> <ManyArgs> | \lambda

	<ManyArgs> -> <exp> | <ManyArgs> ',' <exp>



## Gramática para la Implementación

Nota: La gramatica esta presentada en: [Extended Backus-Naur Form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form), esto por simplicidad (el * significa 0 o mas, igual que una expresion regular!)

	<entrada> -> <S>
	
	<S> -> <exp> <Action>  

	<Action> 
		-> Declaration            
		| Assignment             

	Declaration
		-> Type Assignment        

	Type 
		-> int         
		| bool        
		| lazy Type   

	Assignment 
		-> TkId ':=' <exp> 

	<exp> -> <p0>

    <p0>  -> <p1> ('&&' <p1>)*
    <p1>  -> <p2> ('||' <p2>)*  
    <p2>  -> <p3> (<p2Ops> <p3>)*
    <p3>  -> <p4> (<p3Ops> <p4>)*
    <p4>  -> <p5> (<p4Ops> <p5>)*
    <p5>  -> <p6> (<p5Ops> <p6>)*
    <p6>  -> <optUnary> <p7> (<p6Ops> <p7>)*
    <p7>  -> '(' <exp> ')' | ''' <exp> ''' | integer | boolean | <idOrFapp>

    <p2Ops> -> '=' | '<>'
    <p3Ops> -> '>' | '<' | '>=' | '<='
    <p4Ops> -> '+' | '-'
    <p5Ops> -> '*' | '%'
    <p6Ops> -> '^'

    <optUnary> -> '!' | '-' | '+' | \lambda

    <idOrFapp> -> string <optArgs>

    <optArgs> -> '(' <Args> ')' | \lambda

    <Args> -> <NEArgs> | \lambda
    <NEArgs> -> <exp> (',' NEArgs)*

Notemos que la gramatica asume que TODOS los operadores son asociativos a izquierda, esto esta perfectamente bien puesto que la asociatividad solo determina el orden en que se deben operar las cosas, no si una frase es sintacticamente valida.

En nuestra implementacion interna, esta gramatica parsea a una estructura intermedia que es de tipo: `exp -> [(Tone,exp)]` (asi es, una funcion!), la cual es transformada inmediatamente a algo de tipo arbol sintactico mediante una funcion auxiliar (`toETree`), la cual si tomara en cuenta la asociatividad de cada operador. Quizas esto sea mejor visto en un ejemplo: el parseo de la expresion `true && false && true` se llevaria a cabo de la siguiente manera:

```
   <p0> 
-> <p1> ('&&' <p1>)*
-> <p2> ('||' <p2>)*    ('&&' <p1>)*
-> <p3> (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> <p4> (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> <p5> (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> <p6> (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> <optUnary> <p7> (<p6Ops> <p7>)* (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> \lambda    <p7> (<p6Ops> <p7>)* (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> <p7> (<p6Ops> <p7>)* (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> true (<p6Ops> <p7>)* (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> true (<p5Ops> <p6>)* (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> true (<p4Ops> <p5>)* (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> true (<p3Ops> <p3>)* (<p2Ops> <p3>)* ('||' <p2>)*   ('&&' <p1>)*
-> true (<p2Ops> <p3>)* ('||' <p2>)* ('&&' <p1>)*
-> true ('||' <p2>)* ('&&' <p1>)*
-> true ('&&' <p1>)*
// Aqui  un combinador traduce ('&&' <p1>)* a una lista: ['&&' <p1>, '&&' <p1>,...]
-> true ['&&' <p1>, ('&&' <p1>)*]
-> ...
-> true ['&&' false, '&&' true]
// Esta es la representacion intermedia, ahora una funcion auxiliar parseara esta representacion a un arbol con la asociatividad correcta
-> toExprTree true ['&&' false, '&&' true]
-> And True (toExprTree false ['&&',true])
-> And True (And False (toExprTree True []))
-> And True (And False True) 
// y tada! generamos la asociatividad correcta incluso si la gramatica asume que todo asocia a izquierda!
```

Se preguntara por que tanto trajin? Por que simplemente no utilizar producciones de la forma: `E -> T && E`? Y la razon es porque en parsec, el codigo queda mucho mas conciso de esa forma, absolutamente TODAS las producciones de expresiones (excepto quizas las dos ultimas) tienen el mismo patron:

```haskell
pP4 :: SP m Expr
pP4 = toETree <$> pP5 <*> many ((,) <$> p4Ops <*> pP5)
    where
        p4Ops = [lista de operadores del nivel de precedencia 4]
```

Este patron revela que se podria codear sin mucho problema un parser generator al estilo de yacc!