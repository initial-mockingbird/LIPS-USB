
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

	%right '||' 
	%right '&&' 
	%nonassoc '=' '<>'
	%nonassoc '>' '<' '>=' '<='
	%left '+' '-'
	%left '*' '%'
	%nonassoc NEG PLS '!'
	%right '^'

	<entrada> -> <S>
	
	<S> 
		-> <exp>                      
		| <Action>                   
		| <exp>    ';' S        (Adicional al lenguaje: permite secuenciar operaciones)      
		| <Action> ';' S        
		| <Action> ';' \lambda     
		| <exp>    ';' \lambda     


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

	<exp>
		-> '-' <exp>        
		| '+' <exp> 
		| '!' <exp>                  
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
		| string                      
		| '(' <exp> ')'              
		| ''' <exp> '''              
		| <FApp>
		| <Constant>


	<Constant>
		-> true                      
		| false                     
		| integer                     

	<FApp>
		-> string '(' <Args> ')'         

	<Args>
		-> 
		| <NEArgs>                    

	<NEArgs> 
		-> <exp>                      
		| <NEArgs>  ',' <exp>          

 