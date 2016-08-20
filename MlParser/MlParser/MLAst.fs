module MLAst

type id = ID of string

type label = Label of string

type compoundId = CompundId of list<id>

type signature = Signature

type functorBinding = FunctorBinding

type functorDeclaration = FunctorDeclaration of list<functorBinding>

type signatureDeclaration = SignatureDeclaration of list<id * signature>

type objectDeclaration = 
    Declaration | StructureDeclaration | LocalDeclaration

type pattern = 
    AtomicPattern
    | NamedPattern
    | CompoundPattern
    | TypedPattern
    | NamedTypeAsPattern

type idType = 
    Identifier of compoundId 
    | IdentifierWithType of ``type`` * compoundId 
    | IdentifierWithTypes of list<``type``> * compoundId

and ``type`` = 
    TypeVar of id
    | IdType of idType
    | ProductType of ``type`` * ``type``
    | FunctionType of ``type`` * ``type``
    | LabeledType of list<label * ``type``>
    | SubType

type constant = Const

type compoundName = CompoundName

type infixOp = InfixOperator of id

type dataTypeBinding = DataTypeBinding

type typeBinding = TypeBinding

type ``exception`` = Exception

type ``match`` = Match of list<pattern * expression>

and functionHeading = FunctionHeading

and declaration =
    NullDeclaration
    | ValDeclaration of list<bool * pattern * expression> // boolean value indicates the rpesence of a recursion signifier
    | FunDeclaration of list<functionHeading * ``type`` * expression>
    | TypeDeclaration of typeBinding
    | DataTypeDeclaration of dataTypeBinding * option<typeBinding>
    | AbsTypeDeclaration of dataTypeBinding * option<typeBinding> * declaration
    | ExceptionDeclaration of list<id * ``exception``>
    | LocalDeclaration of declaration * declaration
    | OpenDeclaration of list<compoundName>
    | InfixDeclaration of option<int> * list<id>
    | InfixrDeclaration of option<int> * list<id>
    | NonfixDeclaration of list<id>
    | DeclarationList of list<declaration>

and atomicExpression = 
    CompoundName of compoundName
    | Constant of constant
    | ParenExpression of list<expression>
    | BracketList of list<expression>
    | LabeledExpressionList of list<label * expression>
    | HashLabel of label
    | ExpressionList of list<expression>
    | LetExpression of declaration * list<expression>

and infixExpression = AtomicExpression of list<atomicExpression> | CompundExpr of infixExpression * infixOp * infixExpression

and compoundExpression = AndAlsoExpression of expression * expression | OrElseExpression of expression * expression    

and expression = 
    InfixExpression of infixExpression
    | TypeExpression of expression * ``type``
    | CompoundExpression of compoundExpression
    | HandleExpression of expression * ``match``
    | RaiseExpression of expression
    | IfExpression of expression * expression * expression
    | WhileExpression of expression * expression
    | CaseExpression of expression * ``match``
    | FnExpression of ``match``

and tLDeclaration = 
    Expression of expression
    | ObjectDeclaration of objectDeclaration
    | SignatureDeclaration of signatureDeclaration
    | FunctorDeclaration of functorDeclaration

type program = Program of list<tLDeclaration>
