
# Gramática de LIPS-USB

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

    logicOr:
        logicOr || logicAnd
        
    logicAnd:
        logicAnd && equality

    equality
        equality = comparison
        equality <> comparison

    comparison
        comparison < expression
        comparison <= expression
        comparison >= expression
        comparison > expression

    expression:
        | expression + term
        | expression - term
        | term

    term:
        | term * unaryExpression
        | term / unaryExpression
        | term % unaryExpression
        | unaryExpression

    unaryExpression:
        | ! unaryExpression
        | + unaryExpression
        | - unaryExpression
        | powExpression

    powExpression:
        | factor ^ powExpression
        | factor

    factor:
        ( E )
        | TkNum
        | TkId

   

 