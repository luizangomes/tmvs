module Types where


type FunTable = [(Int,Int)] -- for use in code generation

---------------------
-- TM INSTRUCTIONS --
---------------------

data Instruction = RO ROInstruction Int Int Int String | RM RMInstruction Int Int Int String

instance Show Instruction where
	show (RO roi r s t comment) = (show roi) ++ "\t\t" ++ (show r) ++ "," ++ (show s) ++ "," ++ (show t) ++ "\t\t" ++ comment
	show (RM rmi r d s comment) = (show rmi) ++ "\t\t" ++ (show r) ++ "," ++ (show d) ++ "(" ++ (show s) ++ ")\t\t" ++ comment

data ROInstruction = HALT
        | IN
		| OUT
		| ADD
		| SUB
		| MUL
		| DIV
	deriving (Show,Eq)

data RMInstruction = LD
        | LDA
		| LDC
		| ST
		| JLT
		| JLE
		| JGE
		| JGT
		| JEQ
		| JNE
	deriving (Show,Eq)

isROinstruction (RO _ _ _ _ _) = True
isROinstruction _ = False

isRMinstruction (RM _ _ _ _ _) = True
isRMinstruction _ = False    
	
------------------
-- SYMBOL TABLE --
------------------

type SymbolTable = [(Int, Int, [Decl])]


-----------------
-- SYNTAX TREE --
-----------------

type SyntaxTree = [Decl]

data Decl = VarDecl Type Token Int Level Int
                -- Var type, (ID, row, col), array size (=0 if scalar, otherwise array), level, offset				  
          | FunDecl Type Token [Decl] Stmt Int
		        -- Return type, (ID, row, col), decl list, compound stmt, address
		  | NullDecl
	deriving (Eq)
	
data Stmt = ExpStmt Exp
		  | CompStmt [Decl] [Stmt]
		  | IfStmt Exp Stmt Stmt
		  | WhileStmt Exp Stmt
		  | ReturnStmt Exp
		  | NullStmt
	deriving (Eq)
		  
data Exp = AssignExp Exp Exp
         | OpExp Exp Op Exp
         | IdExp Token Decl
		 | ArrayExp Token Decl Exp
		 | CallExp Token Decl [Exp]
		 | NumExp Int
		 | NullExp
	deriving (Eq)
	
data Op = LtOp
        | LeOp
		| GtOp
		| GeOp
		| EqOp
		| NeqOp
		| PlusOp
		| SubtractOp
		| MultOp
		| DivOp
	deriving (Eq)
         		 		  
data Type = TypeVoid
          | TypeInt
		  | TypeArray
		  | TypeParray
	deriving (Eq)
		  
data Level = Global | Local
	deriving (Eq)		  

-- Printing	

instance Show Decl where		 
	show (VarDecl varType id size level offset) = 
		( "Var, " 
		++ (show varType) ++ ", " ++ (show id) ++ ", size:" ++ (show size) 
		++ ", " ++ (show level) ++ ", offset:" ++ (show offset) )		

	show (FunDecl retType id declList compStmt address) = 
		( "Fun, "
		++ (show retType) ++ ", " ++ (show id) 
		++ ", params:(" ++ (printDeclListFlat declList) ++ "), address:" ++ (show address) )	
		
	show (NullDecl) = "NullDecl"
	
	
printDeclListFlat [] = ""
printDeclListFlat ((VarDecl TypeInt id _ _ _):ds) = 
	(show TypeInt) ++ " " ++ (show id) ++ seperator ++ (printDeclListFlat ds) where
		seperator = if (null ds) then "" else ", "
printDeclListFlat ((VarDecl TypeParray id _ _ _):ds) = 
	(show TypeParray) ++ " " ++ (show id) ++ "[]" ++ seperator ++ (printDeclListFlat ds) where
		seperator = if (null ds) then "" else ", "
	
	
instance Show Op where		 
	show (LtOp) = "<"
	show (LeOp) = "<="
	show (GtOp) = ">"
	show (GeOp) = ">="
	show (EqOp) = "=="
	show (NeqOp) = "!="	
	show (PlusOp) = "+"	
	show (SubtractOp) = "-"	
	show (MultOp) = "*"	
	show (DivOp) = "/"	
	
instance Show Type where		 
	show (TypeVoid) = "Void"
	show (TypeInt) = "Int"
	show (TypeArray) = "Array"
	show (TypeParray) = "Parray"
	
instance Show Level where
	show (Global) = "Global"
	show (Local) = "Local"	
	
-- Helpers	
	
liftJust (Just a) = a	

getDeclName (VarDecl _ (Id name,_,_) _ _ _) = name
getDeclName (FunDecl _ (Id name,_,_) _ _ _) = name	
	
------------	
-- TOKENS --
------------	

type Token = (TokenClass, Int, Int)

data TokenClass = 
             Id String		-- Identifier
		   | Num Int		-- Number
		   
		   | Else
		   | If
		   | Int
		   | Return
		   | Void 
		   | While
		   
		   | Plus
		   | Subtract
		   | Mult
		   | Div
		   | Lt				-- Less than 			<
		   | Gt				-- Greater than 		>
		   | Assign	   		-- Assignment			=
		   | Semi			-- Semicolon			;
		   | Comma			-- Comma				,
		   | Lparen			-- Left Parentheses 	)
		   | Rparen			-- Right Parentheses 	)
		   | Lbracket		-- Left Bracket			[
		   | Rbracket		-- Right Bracket		]
		   | Lbrace			-- Left Brace			{
		   | Rbrace			-- Right Brace			}
		   
		   | Le				-- Less than or eq		<=
		   | Ge				-- Greater than or eq	>=
		   | Neq			-- Not equal			!=
		   | Eq		   		-- Equal				==
		   
		   | EOF
	deriving (Eq)
		   
instance Show TokenClass where
	show (Id str) = "ID: " ++ str
	show (Num num) = "NUM: " ++ (show num)
	
	show (Else) = "ELSE"
	show (If) = "IF"
	show (Int) = "INT"
	show (Return) = "RETURN"
	show (Void) = "VOID"
	show (While) = "WHILE"

	show (Plus) = "PLUS"
	show (Subtract) = "SUBTRACT"	
	show (Mult) = "MULT"	
	show (Div) = "DIV"	
	show (Lt) = "LT"	
	show (Gt) = "GT"	
	show (Assign) = "ASSIGN"
	show (Semi) = "SEMI"	
	show (Comma) = "COMMA"	
	show (Lparen) = "LPAREN"	
	show (Rparen) = "RPAREN"	
	show (Lbracket) = "LBRACKET"	
	show (Rbracket) = "RBRACKET"		
	show (Lbrace) = "LBRACE"		
	show (Rbrace) = "RBRACE"		
	
	show (Le) = "LE"	
	show (Ge) = "GE"		
	show (Neq) = "NEQ"		
	show (Eq) = "EQUAL"		
	
	show (EOF) = "EOF"
